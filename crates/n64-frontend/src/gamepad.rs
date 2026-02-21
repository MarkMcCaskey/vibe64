use gilrs::{Axis, Button, EventType, GamepadId, Gilrs};
use n64_core::memory::pif::buttons as n64_buttons;

#[derive(Default, Clone, Copy)]
pub struct GamepadState {
    pub buttons: u16,
    pub stick_x: i8,
    pub stick_y: i8,
}

pub struct GamepadInput {
    gilrs: Gilrs,
    active: Option<GamepadId>,
    deadzone: f32,
}

impl GamepadInput {
    pub fn new() -> Option<Self> {
        let gilrs = match Gilrs::new() {
            Ok(g) => g,
            Err(e) => {
                log::warn!("Gamepad init failed: {}", e);
                return None;
            }
        };

        let mut gp = Self {
            gilrs,
            active: None,
            deadzone: 0.20,
        };
        gp.pick_first_connected();
        Some(gp)
    }

    pub fn poll(&mut self) -> GamepadState {
        while let Some(ev) = self.gilrs.next_event() {
            match ev.event {
                EventType::Connected => {
                    let name = self.gilrs.gamepad(ev.id).name().to_string();
                    if self.active.is_none() {
                        self.active = Some(ev.id);
                        log::info!("Gamepad connected: {} (selected)", name);
                    } else {
                        log::info!("Gamepad connected: {}", name);
                    }
                }
                EventType::Disconnected => {
                    if self.active == Some(ev.id) {
                        let name = self.gilrs.gamepad(ev.id).name().to_string();
                        log::info!("Gamepad disconnected: {}", name);
                        self.active = None;
                        self.pick_first_connected();
                    }
                }
                _ => {}
            }
        }

        let Some(id) = self.active else {
            return GamepadState::default();
        };

        let g = self.gilrs.gamepad(id);
        let mut out = GamepadState::default();

        if g.is_pressed(Button::South) {
            out.buttons |= n64_buttons::A;
        }
        if g.is_pressed(Button::East) {
            out.buttons |= n64_buttons::B;
        }
        if g.is_pressed(Button::Start) {
            out.buttons |= n64_buttons::START;
        }
        if g.is_pressed(Button::LeftTrigger) {
            out.buttons |= n64_buttons::L;
        }
        if g.is_pressed(Button::RightTrigger) {
            out.buttons |= n64_buttons::R;
        }
        if g.is_pressed(Button::LeftTrigger2) || g.is_pressed(Button::RightTrigger2) {
            out.buttons |= n64_buttons::Z;
        }

        if g.is_pressed(Button::DPadUp) {
            out.buttons |= n64_buttons::D_UP;
        }
        if g.is_pressed(Button::DPadDown) {
            out.buttons |= n64_buttons::D_DOWN;
        }
        if g.is_pressed(Button::DPadLeft) {
            out.buttons |= n64_buttons::D_LEFT;
        }
        if g.is_pressed(Button::DPadRight) {
            out.buttons |= n64_buttons::D_RIGHT;
        }

        // Right stick as C-buttons.
        let rs_x = g.value(Axis::RightStickX);
        let rs_y = g.value(Axis::RightStickY);
        if rs_y <= -0.5 {
            out.buttons |= n64_buttons::C_UP;
        }
        if rs_y >= 0.5 {
            out.buttons |= n64_buttons::C_DOWN;
        }
        if rs_x <= -0.5 {
            out.buttons |= n64_buttons::C_LEFT;
        }
        if rs_x >= 0.5 {
            out.buttons |= n64_buttons::C_RIGHT;
        }

        out.stick_x = axis_to_n64(g.value(Axis::LeftStickX), self.deadzone);
        out.stick_y = axis_to_n64(g.value(Axis::LeftStickY), self.deadzone);

        out
    }

    fn pick_first_connected(&mut self) {
        if let Some((id, gamepad)) = self.gilrs.gamepads().next() {
            self.active = Some(id);
            log::info!("Using gamepad: {}", gamepad.name());
        }
    }
}

fn axis_to_n64(v: f32, deadzone: f32) -> i8 {
    let mut x = if v.abs() < deadzone { 0.0 } else { v };
    x = x.clamp(-1.0, 1.0);
    // N64 stick practical range is around +/-80.
    (x * 80.0) as i8
}
