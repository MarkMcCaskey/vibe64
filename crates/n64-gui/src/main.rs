mod app;
mod config;

fn main() {
    env_logger::init();

    let rom_path = std::env::args().nth(1).map(std::path::PathBuf::from);

    let config = config::EmulatorConfig::load();

    let native_options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([
                n64_frontend_common::N64_WIDTH as f32 * config.window_scale as f32,
                n64_frontend_common::N64_HEIGHT as f32 * config.window_scale as f32 + 24.0,
            ])
            .with_min_inner_size([
                n64_frontend_common::N64_WIDTH as f32,
                n64_frontend_common::N64_HEIGHT as f32 + 24.0,
            ]),
        ..Default::default()
    };

    eframe::run_native(
        "N64",
        native_options,
        Box::new(move |cc| Ok(Box::new(app::EmulatorApp::new(cc, config, rom_path)))),
    )
    .expect("Failed to run eframe");
}
