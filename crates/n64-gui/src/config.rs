use std::collections::HashMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

const MAX_RECENT_ROMS: usize = 20;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EmulatorConfig {
    #[serde(default = "default_window_scale")]
    pub window_scale: u32,
    #[serde(default)]
    pub audio_muted: bool,
    #[serde(default)]
    pub save_slot: u8,
    #[serde(default)]
    pub fullscreen: bool,
    #[serde(default)]
    pub recent_roms: Vec<PathBuf>,
    #[serde(default)]
    pub last_rom_dir: Option<PathBuf>,
    #[serde(default = "default_input_mapping")]
    pub input_mapping: HashMap<String, String>,
}

fn default_window_scale() -> u32 {
    2
}

fn default_input_mapping() -> HashMap<String, String> {
    let mut m = HashMap::new();
    m.insert("A".into(), "X".into());
    m.insert("B".into(), "Z".into());
    m.insert("Z".into(), "Space".into());
    m.insert("Start".into(), "Enter".into());
    m.insert("L".into(), "ShiftLeft".into());
    m.insert("R".into(), "ShiftRight".into());
    m.insert("C-Up".into(), "I".into());
    m.insert("C-Down".into(), "K".into());
    m.insert("C-Left".into(), "J".into());
    m.insert("C-Right".into(), "L".into());
    m.insert("D-Up".into(), "W".into());
    m.insert("D-Down".into(), "S".into());
    m.insert("D-Left".into(), "A".into());
    m.insert("D-Right".into(), "D".into());
    m
}

impl Default for EmulatorConfig {
    fn default() -> Self {
        Self {
            window_scale: default_window_scale(),
            audio_muted: false,
            save_slot: 0,
            fullscreen: false,
            recent_roms: Vec::new(),
            last_rom_dir: None,
            input_mapping: default_input_mapping(),
        }
    }
}

impl EmulatorConfig {
    fn config_path() -> Option<PathBuf> {
        dirs::config_dir().map(|d| d.join("n64emu").join("config.toml"))
    }

    pub fn load() -> Self {
        let Some(path) = Self::config_path() else {
            return Self::default();
        };
        match std::fs::read_to_string(&path) {
            Ok(contents) => match toml::from_str(&contents) {
                Ok(config) => {
                    log::info!("Loaded config from {}", path.display());
                    config
                }
                Err(e) => {
                    log::warn!("Failed to parse config {}: {}", path.display(), e);
                    Self::default()
                }
            },
            Err(_) => Self::default(),
        }
    }

    pub fn save(&self) {
        let Some(path) = Self::config_path() else {
            return;
        };
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        match toml::to_string_pretty(self) {
            Ok(contents) => {
                if let Err(e) = std::fs::write(&path, contents) {
                    log::warn!("Failed to write config {}: {}", path.display(), e);
                }
            }
            Err(e) => {
                log::warn!("Failed to serialize config: {}", e);
            }
        }
    }

    pub fn add_recent_rom(&mut self, path: &Path) {
        let canonical = path.to_path_buf();
        self.recent_roms.retain(|p| p != &canonical);
        self.recent_roms.insert(0, canonical);
        self.recent_roms.truncate(MAX_RECENT_ROMS);
        if let Some(parent) = path.parent() {
            self.last_rom_dir = Some(parent.to_path_buf());
        }
        self.save();
    }
}
