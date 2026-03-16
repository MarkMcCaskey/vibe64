use std::path::Path;
use std::time::Instant;

use eframe::egui;
use n64_core::memory::pif::buttons as n64_buttons;
use n64_frontend_common::blit::{self, N64_HEIGHT, N64_WIDTH};
use n64_frontend_common::{audio, debug_overlay, gamepad};

use crate::config::EmulatorConfig;

const FB_BYTES: usize = N64_WIDTH as usize * N64_HEIGHT as usize * 4;

const SPEED_STEP: u32 = 25;
const MIN_SPEED: u32 = 25;
const MAX_SPEED: u32 = 400;
const DEFAULT_SPEED: u32 = 100;
const SAVE_FLUSH_INTERVAL: u64 = 300; // ~5 seconds at 60fps

#[derive(PartialEq, Eq)]
enum EmuState {
    Idle,
    Running,
    Paused,
}

struct Toast {
    message: String,
    until: Instant,
}

pub struct EmulatorApp {
    // Emulation
    n64: Option<n64_core::N64>,
    state: EmuState,

    // Framebuffer
    fb_buf: Vec<u8>,
    fb_texture: Option<egui::TextureHandle>,

    // Subsystems
    audio: Option<audio::AudioOutput>,
    gamepad: Option<gamepad::GamepadInput>,

    // Input state
    keyboard_buttons: u16,
    keyboard_stick_x: i8,
    keyboard_stick_y: i8,
    arrow_up: bool,
    arrow_down: bool,
    arrow_left: bool,
    arrow_right: bool,

    // GUI state
    config: EmulatorConfig,
    show_settings: bool,
    show_save_states: bool,
    toasts: Vec<Toast>,

    // Timing
    speed_percent: u32,
    speed_frame_budget: f32,
    last_save_flush_frame: u64,
}

impl EmulatorApp {
    pub fn new(
        _cc: &eframe::CreationContext<'_>,
        config: EmulatorConfig,
        rom_path: Option<std::path::PathBuf>,
    ) -> Self {
        let audio = audio::AudioOutput::new();
        let gamepad = gamepad::GamepadInput::new();

        let mut app = Self {
            n64: None,
            state: EmuState::Idle,
            fb_buf: vec![0u8; FB_BYTES],
            fb_texture: None,
            audio,
            gamepad,
            keyboard_buttons: 0,
            keyboard_stick_x: 0,
            keyboard_stick_y: 0,
            arrow_up: false,
            arrow_down: false,
            arrow_left: false,
            arrow_right: false,
            config,
            show_settings: false,
            show_save_states: false,
            toasts: Vec::new(),
            speed_percent: DEFAULT_SPEED,
            speed_frame_budget: 0.0,
            last_save_flush_frame: 0,
        };

        if let Some(path) = rom_path {
            app.load_rom(&path);
        }

        app
    }

    fn load_rom(&mut self, path: &Path) {
        self.flush_saves();

        match n64_core::N64::new(path) {
            Ok(mut n64) => {
                if let Some(audio) = &self.audio {
                    n64.set_audio_callback(audio.make_callback());
                }
                let name = n64.bus.cart.header.name.clone();
                self.n64 = Some(n64);
                self.state = EmuState::Running;
                self.fb_buf.fill(0);
                self.fb_texture = None;
                self.speed_frame_budget = 0.0;
                self.last_save_flush_frame = 0;
                self.config.add_recent_rom(path);
                self.toast(format!("Loaded: {}", name.trim()));
            }
            Err(e) => {
                self.toast(format!("Error: {}", e));
            }
        }
    }

    fn toast(&mut self, message: String) {
        self.toasts.push(Toast {
            message,
            until: Instant::now() + std::time::Duration::from_secs(3),
        });
    }

    fn save_state(&mut self) {
        let slot = self.config.save_slot;
        if let Some(n64) = &self.n64 {
            match n64.save_state(slot) {
                Ok(()) => self.toast(format!("State saved to slot {}", slot)),
                Err(e) => self.toast(format!("Save error: {}", e)),
            }
        }
    }

    fn load_state(&mut self) {
        let slot = self.config.save_slot;
        if let Some(n64) = &mut self.n64 {
            match n64.load_state(slot) {
                Ok(()) => self.toast(format!("State loaded from slot {}", slot)),
                Err(e) => self.toast(format!("Load error: {}", e)),
            }
        }
    }

    fn save_state_slot(&mut self, slot: u8) {
        self.config.save_slot = slot;
        self.save_state();
    }

    fn load_state_slot(&mut self, slot: u8) {
        self.config.save_slot = slot;
        self.load_state();
    }

    fn open_rom_dialog(&mut self) {
        let mut dialog = rfd::FileDialog::new().add_filter("N64 ROM", &["z64", "n64", "v64"]);
        if let Some(dir) = &self.config.last_rom_dir {
            dialog = dialog.set_directory(dir);
        }
        if let Some(path) = dialog.pick_file() {
            self.load_rom(&path);
        }
    }

    fn window_title(&self) -> String {
        match &self.n64 {
            Some(n64) => {
                let name = n64.bus.cart.header.name.trim();
                if name.is_empty() {
                    "N64".to_string()
                } else {
                    format!("N64 - {}", name)
                }
            }
            None => "N64".to_string(),
        }
    }

    fn flush_saves(&mut self) {
        if let Some(n64) = &mut self.n64 {
            n64.save_eeprom();
            n64.save_flash();
            n64.save_sram();
        }
    }

    fn process_input(&mut self, ctx: &egui::Context) {
        if ctx.wants_keyboard_input() {
            return;
        }

        ctx.input(|input| {
            // N64 button mapping
            const KEY_MAP: &[(egui::Key, u16)] = &[
                (egui::Key::X, n64_buttons::A),
                (egui::Key::Z, n64_buttons::B),
                (egui::Key::Space, n64_buttons::Z),
                (egui::Key::Enter, n64_buttons::START),
                (egui::Key::I, n64_buttons::C_UP),
                (egui::Key::K, n64_buttons::C_DOWN),
                (egui::Key::J, n64_buttons::C_LEFT),
                (egui::Key::L, n64_buttons::C_RIGHT),
                (egui::Key::W, n64_buttons::D_UP),
                (egui::Key::S, n64_buttons::D_DOWN),
                (egui::Key::A, n64_buttons::D_LEFT),
                (egui::Key::D, n64_buttons::D_RIGHT),
            ];

            self.keyboard_buttons = 0;
            for &(key, btn) in KEY_MAP {
                if input.key_down(key) {
                    self.keyboard_buttons |= btn;
                }
            }

            // Shoulder buttons via modifiers
            if input.modifiers.shift {
                self.keyboard_buttons |= n64_buttons::L | n64_buttons::R;
            }

            // Arrow keys for analog stick
            self.arrow_up = input.key_down(egui::Key::ArrowUp);
            self.arrow_down = input.key_down(egui::Key::ArrowDown);
            self.arrow_left = input.key_down(egui::Key::ArrowLeft);
            self.arrow_right = input.key_down(egui::Key::ArrowRight);

            self.keyboard_stick_y = match (self.arrow_up, self.arrow_down) {
                (true, false) => 80,
                (false, true) => -80,
                _ => 0,
            };
            self.keyboard_stick_x = match (self.arrow_right, self.arrow_left) {
                (true, false) => 80,
                (false, true) => -80,
                _ => 0,
            };
        });
    }

    fn sync_controller(&mut self) {
        let mut gp = gamepad::GamepadState::default();
        if let Some(gamepad) = &mut self.gamepad {
            gp = gamepad.poll();
        }

        if let Some(n64) = &mut self.n64 {
            let ctrl = &mut n64.bus.pif.controller;
            ctrl.buttons = self.keyboard_buttons | gp.buttons;
            ctrl.stick_x = gamepad::combine_axis(self.keyboard_stick_x, gp.stick_x);
            ctrl.stick_y = gamepad::combine_axis(self.keyboard_stick_y, gp.stick_y);
        }
    }

    fn handle_shortcuts(&mut self, ctx: &egui::Context) {
        // Read all shortcut key states in one call.
        let shortcuts = ctx.input(|i| Shortcuts {
            open_rom: i.modifiers.command && i.key_pressed(egui::Key::O),
            quit: i.modifiers.command && i.key_pressed(egui::Key::Q),
            reset: i.modifiers.command && i.key_pressed(egui::Key::R),
            toggle_pause: i.key_pressed(egui::Key::Escape),
            quick_save: i.key_pressed(egui::Key::F5),
            quick_load: i.key_pressed(egui::Key::F7),
            slot_up: i.key_pressed(egui::Key::F6),
            slot_down: i.key_pressed(egui::Key::F8),
            speed_up: !i.modifiers.command && i.key_pressed(egui::Key::Equals),
            speed_down: !i.modifiers.command && i.key_pressed(egui::Key::Minus),
            speed_reset: !i.modifiers.command && i.key_pressed(egui::Key::Num0),
            screenshot: i.key_pressed(egui::Key::P),
            toggle_mute: i.key_pressed(egui::Key::M),
            debug_f1: i.key_pressed(egui::Key::F1),
            debug_f2: i.key_pressed(egui::Key::F2),
            debug_f3: i.key_pressed(egui::Key::F3),
            debug_f4: i.key_pressed(egui::Key::F4),
        });

        if shortcuts.quit {
            self.flush_saves();
            ctx.send_viewport_cmd(egui::ViewportCommand::Close);
            return;
        }

        if shortcuts.open_rom {
            self.open_rom_dialog();
        }
        if shortcuts.reset {
            if let Some(n64) = &self.n64 {
                let path = n64.rom_path.clone();
                self.load_rom(&path);
            }
        }
        if shortcuts.toggle_pause && self.n64.is_some() {
            self.state = match self.state {
                EmuState::Running => EmuState::Paused,
                EmuState::Paused => EmuState::Running,
                EmuState::Idle => EmuState::Idle,
            };
        }
        if shortcuts.quick_save {
            self.save_state();
        }
        if shortcuts.quick_load {
            self.load_state();
        }
        if shortcuts.slot_up {
            self.config.save_slot = (self.config.save_slot + 1) % 10;
            self.toast(format!("Save slot: {}", self.config.save_slot));
        }
        if shortcuts.slot_down {
            self.config.save_slot = if self.config.save_slot == 0 {
                9
            } else {
                self.config.save_slot - 1
            };
            self.toast(format!("Save slot: {}", self.config.save_slot));
        }
        if shortcuts.speed_up {
            self.speed_percent = (self.speed_percent + SPEED_STEP).min(MAX_SPEED);
            self.toast(format!("Speed: {}%", self.speed_percent));
        }
        if shortcuts.speed_down {
            self.speed_percent = self.speed_percent.saturating_sub(SPEED_STEP).max(MIN_SPEED);
            self.toast(format!("Speed: {}%", self.speed_percent));
        }
        if shortcuts.speed_reset {
            self.speed_percent = DEFAULT_SPEED;
            self.toast(format!("Speed: {}%", self.speed_percent));
        }
        if shortcuts.screenshot {
            if let Some(n64) = &self.n64 {
                blit::save_screenshot(n64);
                self.toast("Screenshot saved".into());
            }
        }
        if shortcuts.toggle_mute {
            if let Some(audio) = &mut self.audio {
                let muted = audio.toggle_mute();
                self.toast(if muted {
                    "Audio muted".into()
                } else {
                    "Audio unmuted".into()
                });
            }
        }

        // Debug overlay keys
        for (pressed, idx) in [
            (shortcuts.debug_f1, 1u8),
            (shortcuts.debug_f2, 2),
            (shortcuts.debug_f3, 3),
            (shortcuts.debug_f4, 4),
        ] {
            if pressed {
                if let Some(n64) = &mut self.n64 {
                    debug_overlay::toggle_debug_overlay(&mut n64.debug, idx);
                }
            }
        }
    }

    fn draw_menu_bar(&mut self, ctx: &egui::Context) {
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                // File menu
                ui.menu_button("File", |ui| {
                    if ui.button("Open ROM...  (Ctrl+O)").clicked() {
                        ui.close_menu();
                        self.open_rom_dialog();
                    }

                    if !self.config.recent_roms.is_empty() {
                        ui.menu_button("Recent ROMs", |ui| {
                            let mut load_path = None;
                            for path in &self.config.recent_roms {
                                let label = path
                                    .file_name()
                                    .map(|n| n.to_string_lossy().to_string())
                                    .unwrap_or_else(|| path.display().to_string());
                                if ui.button(&label).clicked() {
                                    load_path = Some(path.clone());
                                    ui.close_menu();
                                }
                            }
                            if let Some(path) = load_path {
                                self.load_rom(&path);
                            }
                        });
                    }

                    ui.separator();

                    if ui.button("Screenshot  (P)").clicked() {
                        ui.close_menu();
                        if let Some(n64) = &self.n64 {
                            blit::save_screenshot(n64);
                            self.toast("Screenshot saved".into());
                        }
                    }

                    ui.separator();

                    if ui.button("Exit  (Ctrl+Q)").clicked() {
                        self.flush_saves();
                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });

                // Emulation menu
                ui.menu_button("Emulation", |ui| {
                    let has_rom = self.n64.is_some();

                    let pause_label = if self.state == EmuState::Paused {
                        "Resume  (Esc)"
                    } else {
                        "Pause  (Esc)"
                    };
                    if ui
                        .add_enabled(has_rom, egui::Button::new(pause_label))
                        .clicked()
                    {
                        self.state = if self.state == EmuState::Running {
                            EmuState::Paused
                        } else {
                            EmuState::Running
                        };
                        ui.close_menu();
                    }

                    if ui
                        .add_enabled(has_rom, egui::Button::new("Reset  (Ctrl+R)"))
                        .clicked()
                    {
                        if let Some(n64) = &self.n64 {
                            let path = n64.rom_path.clone();
                            ui.close_menu();
                            self.load_rom(&path);
                        }
                    }

                    ui.separator();

                    ui.horizontal(|ui| {
                        ui.label("Speed:");
                        if ui.button("-").clicked() {
                            self.speed_percent =
                                self.speed_percent.saturating_sub(SPEED_STEP).max(MIN_SPEED);
                        }
                        ui.label(format!("{}%", self.speed_percent));
                        if ui.button("+").clicked() {
                            self.speed_percent = (self.speed_percent + SPEED_STEP).min(MAX_SPEED);
                        }
                        if ui.button("100%").clicked() {
                            self.speed_percent = DEFAULT_SPEED;
                        }
                    });
                });

                // Save States menu
                ui.menu_button("Save States", |ui| {
                    let has_rom = self.n64.is_some();

                    if ui
                        .add_enabled(has_rom, egui::Button::new("Quick Save  (F5)"))
                        .clicked()
                    {
                        ui.close_menu();
                        self.save_state();
                    }
                    if ui
                        .add_enabled(has_rom, egui::Button::new("Quick Load  (F7)"))
                        .clicked()
                    {
                        ui.close_menu();
                        self.load_state();
                    }

                    ui.separator();

                    if ui.button("Manage Slots...").clicked() {
                        self.show_save_states = !self.show_save_states;
                        ui.close_menu();
                    }

                    ui.separator();

                    for slot in 0..10u8 {
                        let is_current = slot == self.config.save_slot;
                        let exists = self
                            .n64
                            .as_ref()
                            .map_or(false, |n64| n64.save_state_path(slot).exists());
                        let timestamp = if exists {
                            self.n64
                                .as_ref()
                                .and_then(|n64| {
                                    std::fs::metadata(n64.save_state_path(slot))
                                        .ok()?
                                        .modified()
                                        .ok()
                                        .map(format_timestamp)
                                })
                                .unwrap_or_default()
                        } else {
                            String::new()
                        };

                        let prefix = if is_current { "> " } else { "  " };
                        let suffix = if exists { &timestamp } else { "[empty]" };
                        let label = format!("{}Slot {}  {}", prefix, slot, suffix);

                        if ui.button(&label).clicked() {
                            self.config.save_slot = slot;
                            ui.close_menu();
                        }
                    }
                });

                // Options menu
                ui.menu_button("Options", |ui| {
                    if ui.button("Settings...").clicked() {
                        self.show_settings = !self.show_settings;
                        ui.close_menu();
                    }

                    ui.separator();

                    let mut muted = self.audio.as_ref().map_or(false, |a| a.is_muted());
                    if ui.checkbox(&mut muted, "Mute Audio  (M)").changed() {
                        if let Some(audio) = &self.audio {
                            audio.set_muted(muted);
                        }
                    }

                    ui.separator();

                    ui.menu_button("Window Scale", |ui| {
                        for scale in 1..=4u32 {
                            let label = format!("{}x", scale);
                            if ui
                                .radio(self.config.window_scale == scale, &label)
                                .clicked()
                            {
                                self.config.window_scale = scale;
                                self.config.save();
                                ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(
                                    egui::vec2(
                                        N64_WIDTH as f32 * scale as f32,
                                        N64_HEIGHT as f32 * scale as f32 + 24.0,
                                    ),
                                ));
                                ui.close_menu();
                            }
                        }
                    });
                });

                // Debug menu
                if self.n64.is_some() {
                    ui.menu_button("Debug", |ui| {
                        let flags = self.n64.as_ref().map(|n| &n.debug.flags);
                        let mut show_stats = flags.map_or(false, |f| f.show_stats);
                        let mut show_wire = flags.map_or(false, |f| f.show_wireframe);
                        let mut show_depth = flags.map_or(false, |f| f.show_depth);
                        let mut show_tex = flags.map_or(false, |f| f.show_textures);

                        if ui.checkbox(&mut show_stats, "Stats HUD  (F1)").changed() {
                            if let Some(n64) = &mut self.n64 {
                                debug_overlay::toggle_debug_overlay(&mut n64.debug, 1);
                            }
                        }
                        if ui.checkbox(&mut show_wire, "Wireframe  (F2)").changed() {
                            if let Some(n64) = &mut self.n64 {
                                debug_overlay::toggle_debug_overlay(&mut n64.debug, 2);
                            }
                        }
                        if ui.checkbox(&mut show_depth, "Depth Buffer  (F3)").changed() {
                            if let Some(n64) = &mut self.n64 {
                                debug_overlay::toggle_debug_overlay(&mut n64.debug, 3);
                            }
                        }
                        if ui.checkbox(&mut show_tex, "Texture Info  (F4)").changed() {
                            if let Some(n64) = &mut self.n64 {
                                debug_overlay::toggle_debug_overlay(&mut n64.debug, 4);
                            }
                        }
                    });
                }

                // Help menu
                ui.menu_button("Help", |ui| {
                    if ui.button("About").clicked() {
                        self.toast("N64 Emulator - Rust".into());
                        ui.close_menu();
                    }
                    if ui.button("Keyboard Shortcuts").clicked() {
                        self.toast(
                            "Ctrl+O: Open ROM | F5/F7: Save/Load | Esc: Pause".into(),
                        );
                        ui.close_menu();
                    }
                });
            });
        });
    }

    fn draw_idle_screen(&mut self, ctx: &egui::Context) {
        egui::CentralPanel::default().show(ctx, |ui| {
            ui.vertical_centered(|ui| {
                ui.add_space(40.0);
                ui.heading("N64 Emulator");
                ui.add_space(20.0);

                if ui.button("Open ROM...").clicked() {
                    self.open_rom_dialog();
                }

                ui.add_space(20.0);

                if !self.config.recent_roms.is_empty() {
                    ui.separator();
                    ui.label("Recent ROMs:");
                    ui.add_space(8.0);

                    let mut load_path = None;
                    for path in &self.config.recent_roms {
                        let label = path
                            .file_name()
                            .map(|n| n.to_string_lossy().to_string())
                            .unwrap_or_else(|| path.display().to_string());
                        if ui.link(&label).clicked() {
                            load_path = Some(path.clone());
                        }
                    }
                    if let Some(path) = load_path {
                        self.load_rom(&path);
                    }
                }
            });
        });
    }

    fn draw_emulator_viewport(&mut self, ctx: &egui::Context) {
        egui::CentralPanel::default()
            .frame(egui::Frame::none().fill(egui::Color32::BLACK))
            .show(ctx, |ui| {
                if let Some(texture) = &self.fb_texture {
                    let available = ui.available_size();
                    let fb_w = N64_WIDTH as f32;
                    let fb_h = N64_HEIGHT as f32;
                    let scale = (available.x / fb_w).min(available.y / fb_h).floor().max(1.0);
                    let display_w = fb_w * scale;
                    let display_h = fb_h * scale;

                    // Center the image
                    let pad_x = (available.x - display_w) / 2.0;
                    let pad_y = (available.y - display_h) / 2.0;
                    ui.add_space(pad_y.max(0.0));
                    ui.horizontal(|ui| {
                        ui.add_space(pad_x.max(0.0));
                        ui.image(egui::load::SizedTexture::new(
                            texture.id(),
                            egui::vec2(display_w, display_h),
                        ));
                    });
                }
            });
    }

    fn draw_toasts(&mut self, ctx: &egui::Context) {
        let now = Instant::now();
        self.toasts.retain(|t| now < t.until);
        if self.toasts.is_empty() {
            return;
        }

        egui::Area::new(egui::Id::new("toasts"))
            .anchor(egui::Align2::CENTER_BOTTOM, egui::vec2(0.0, -12.0))
            .interactable(false)
            .show(ctx, |ui| {
                ui.vertical(|ui| {
                    for toast in &self.toasts {
                        let text = egui::RichText::new(&toast.message)
                            .color(egui::Color32::WHITE)
                            .size(14.0);
                        egui::Frame::none()
                            .fill(egui::Color32::from_black_alpha(180))
                            .rounding(4.0)
                            .inner_margin(egui::Margin::symmetric(8.0, 4.0))
                            .show(ui, |ui| {
                                ui.label(text);
                            });
                        ui.add_space(2.0);
                    }
                });
            });
    }

    fn draw_save_state_window(&mut self, ctx: &egui::Context) {
        if !self.show_save_states {
            return;
        }

        let mut open = self.show_save_states;
        let mut action: Option<(u8, bool)> = None; // (slot, is_save)

        egui::Window::new("Save States")
            .open(&mut open)
            .resizable(false)
            .show(ctx, |ui| {
                let has_rom = self.n64.is_some();

                for slot in 0..10u8 {
                    let is_current = slot == self.config.save_slot;
                    let exists = self
                        .n64
                        .as_ref()
                        .map_or(false, |n64| n64.save_state_path(slot).exists());
                    let timestamp = if exists {
                        self.n64
                            .as_ref()
                            .and_then(|n64| {
                                std::fs::metadata(n64.save_state_path(slot))
                                    .ok()?
                                    .modified()
                                    .ok()
                                    .map(format_timestamp)
                            })
                            .unwrap_or_default()
                    } else {
                        "[empty]".to_string()
                    };

                    ui.horizontal(|ui| {
                        let label = format!("Slot {}", slot);
                        if ui.selectable_label(is_current, &label).clicked() {
                            self.config.save_slot = slot;
                        }
                        ui.label(&timestamp);

                        ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                            if ui
                                .add_enabled(has_rom && exists, egui::Button::new("Load"))
                                .clicked()
                            {
                                action = Some((slot, false));
                            }
                            if ui
                                .add_enabled(has_rom, egui::Button::new("Save"))
                                .clicked()
                            {
                                action = Some((slot, true));
                            }
                        });
                    });
                }
            });

        // Execute deferred save/load outside the UI closure
        if let Some((slot, is_save)) = action {
            if is_save {
                self.save_state_slot(slot);
            } else {
                self.load_state_slot(slot);
            }
        }

        self.show_save_states = open;
    }

    fn draw_settings_window(&mut self, ctx: &egui::Context) {
        if !self.show_settings {
            return;
        }

        let mut open = self.show_settings;
        egui::Window::new("Settings")
            .open(&mut open)
            .resizable(false)
            .default_width(300.0)
            .show(ctx, |ui| {
                ui.heading("Video");
                ui.horizontal(|ui| {
                    ui.label("Window Scale:");
                    for scale in 1..=4u32 {
                        if ui
                            .radio(self.config.window_scale == scale, format!("{}x", scale))
                            .clicked()
                        {
                            self.config.window_scale = scale;
                            self.config.save();
                            ctx.send_viewport_cmd(egui::ViewportCommand::InnerSize(egui::vec2(
                                N64_WIDTH as f32 * scale as f32,
                                N64_HEIGHT as f32 * scale as f32 + 24.0,
                            )));
                        }
                    }
                });

                ui.separator();
                ui.heading("Audio");
                let mut muted = self.audio.as_ref().map_or(false, |a| a.is_muted());
                if ui.checkbox(&mut muted, "Mute").changed() {
                    if let Some(audio) = &self.audio {
                        audio.set_muted(muted);
                    }
                }

                ui.separator();
                ui.heading("Input Mapping");
                ui.label("Keyboard controls:");
                egui::Grid::new("input_grid").striped(true).show(ui, |ui| {
                    let mut sorted: Vec<_> = self.config.input_mapping.iter().collect();
                    sorted.sort_by_key(|(k, _)| (*k).clone());
                    for (n64_btn, key) in sorted {
                        ui.label(n64_btn);
                        ui.label(key);
                        ui.end_row();
                    }
                });
            });
        self.show_settings = open;
    }
}

struct Shortcuts {
    open_rom: bool,
    quit: bool,
    reset: bool,
    toggle_pause: bool,
    quick_save: bool,
    quick_load: bool,
    slot_up: bool,
    slot_down: bool,
    speed_up: bool,
    speed_down: bool,
    speed_reset: bool,
    screenshot: bool,
    toggle_mute: bool,
    debug_f1: bool,
    debug_f2: bool,
    debug_f3: bool,
    debug_f4: bool,
}

impl eframe::App for EmulatorApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        ctx.send_viewport_cmd(egui::ViewportCommand::Title(self.window_title()));

        self.handle_shortcuts(ctx);
        self.process_input(ctx);
        self.sync_controller();

        // Run emulation
        if self.state == EmuState::Running {
            if let Some(n64) = &mut self.n64 {
                let speed_mult = self.speed_percent as f32 / 100.0;
                self.speed_frame_budget += speed_mult;
                let frames = self.speed_frame_budget.floor() as u32;
                self.speed_frame_budget -= frames as f32;

                let old_frame = n64.debug.frame_count;
                for _ in 0..frames {
                    n64.run_frame();
                }
                n64.debug.frame_count = old_frame.saturating_add(frames as u64);

                // Blit framebuffer
                blit::blit_framebuffer(n64, &mut self.fb_buf);
                debug_overlay::draw_overlays(&mut self.fb_buf, &mut n64.debug, &n64.bus);

                // Upload texture
                let image = egui::ColorImage::from_rgba_unmultiplied(
                    [N64_WIDTH as usize, N64_HEIGHT as usize],
                    &self.fb_buf,
                );
                self.fb_texture = Some(ctx.load_texture(
                    "n64_fb",
                    image,
                    egui::TextureOptions::NEAREST,
                ));

                // Periodic save flush
                if n64.debug.frame_count / SAVE_FLUSH_INTERVAL
                    != self.last_save_flush_frame / SAVE_FLUSH_INTERVAL
                {
                    n64.save_eeprom();
                    n64.save_flash();
                    n64.save_sram();
                }
                self.last_save_flush_frame = n64.debug.frame_count;
            }

            ctx.request_repaint();
        }

        // Draw UI
        self.draw_menu_bar(ctx);

        if self.state == EmuState::Idle {
            self.draw_idle_screen(ctx);
        } else {
            self.draw_emulator_viewport(ctx);
        }

        self.draw_save_state_window(ctx);
        self.draw_settings_window(ctx);
        self.draw_toasts(ctx);
    }

    fn on_exit(&mut self) {
        self.flush_saves();
        self.config.save();
    }
}

fn format_timestamp(time: std::time::SystemTime) -> String {
    let duration = time
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = duration.as_secs();

    let days = secs / 86400;
    let time_of_day = secs % 86400;
    let hours = time_of_day / 3600;
    let minutes = (time_of_day % 3600) / 60;

    // Date from days since epoch
    let mut y = 1970i64;
    let mut remaining = days as i64;
    loop {
        let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
        let days_in_year = if leap { 366 } else { 365 };
        if remaining < days_in_year {
            break;
        }
        remaining -= days_in_year;
        y += 1;
    }
    let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
    let month_days = [
        31,
        if leap { 29 } else { 28 },
        31,
        30,
        31,
        30,
        31,
        31,
        30,
        31,
        30,
        31,
    ];
    let mut m = 0;
    for &md in &month_days {
        if remaining < md as i64 {
            break;
        }
        remaining -= md as i64;
        m += 1;
    }

    format!(
        "{:04}-{:02}-{:02} {:02}:{:02}",
        y,
        m + 1,
        remaining + 1,
        hours,
        minutes,
    )
}
