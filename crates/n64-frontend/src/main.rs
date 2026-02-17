use std::path::PathBuf;

fn main() {
    env_logger::init();

    let rom_path = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            eprintln!("Usage: n64-frontend <rom_path>");
            std::process::exit(1);
        });

    let mut n64 = match n64_core::N64::new(&rom_path) {
        Ok(n64) => n64,
        Err(e) => {
            eprintln!("Failed to load ROM: {}", e);
            std::process::exit(1);
        }
    };

    println!("Starting emulation...");

    // Main loop: run frames
    // For now this just runs the CPU. Later we'll add:
    // - Window/framebuffer rendering (VI output)
    // - Audio output (AI output)
    // - Input polling (SI/PIF)
    loop {
        n64.run_frame();
    }
}
