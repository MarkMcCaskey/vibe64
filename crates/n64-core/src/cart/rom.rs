use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;

/// ROM byte order format, detected from the first 4 bytes.
/// See doc/n64_architecture.md for details.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RomFormat {
    /// .z64 — Big-endian (native N64 byte order). Magic: 0x80371240
    BigEndian,
    /// .v64 — Byte-swapped (each 16-bit pair has bytes flipped). Magic: 0x37804012
    ByteSwapped,
    /// .n64 — Little-endian (each 32-bit word is reversed). Magic: 0x40123780
    LittleEndian,
}

/// Parsed ROM header (first 0x40 bytes of the ROM).
#[derive(Debug, Clone)]
pub struct RomHeader {
    pub format: RomFormat,
    pub clock_rate: u32,
    pub entry_point: u32,
    pub release: u32,
    pub crc1: u32,
    pub crc2: u32,
    pub name: String,
    pub game_code: [u8; 4],
    pub version: u8,
}

#[derive(Debug, thiserror::Error)]
pub enum RomError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Unknown ROM format (magic bytes: {0:02X} {1:02X} {2:02X} {3:02X})")]
    UnknownFormat(u8, u8, u8, u8),
    #[error("ROM too small (need at least 64 bytes for header, got {0})")]
    TooSmall(usize),
}

/// Load a ROM from disk: detect format, normalize, parse header.
pub fn load_rom(path: &Path) -> Result<(RomHeader, Vec<u8>), RomError> {
    let mut file = File::open(path)?;

    // Read magic bytes
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    let format = detect_format(magic)?;

    // Read entire ROM
    file.seek(SeekFrom::Start(0))?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    if data.len() < 0x40 {
        return Err(RomError::TooSmall(data.len()));
    }

    // Normalize to big-endian
    normalize(&mut data, format);

    // Parse header
    let header = parse_header(&data, format)?;

    Ok((header, data))
}

pub fn detect_format(magic: [u8; 4]) -> Result<RomFormat, RomError> {
    match magic {
        [0x80, 0x37, 0x12, 0x40] => Ok(RomFormat::BigEndian),
        [0x37, 0x80, 0x40, 0x12] => Ok(RomFormat::ByteSwapped),
        [0x40, 0x12, 0x37, 0x80] => Ok(RomFormat::LittleEndian),
        _ => Err(RomError::UnknownFormat(
            magic[0], magic[1], magic[2], magic[3],
        )),
    }
}

pub fn normalize(data: &mut Vec<u8>, format: RomFormat) {
    match format {
        RomFormat::BigEndian => (),
        RomFormat::ByteSwapped => {
            for chunk in data.chunks_exact_mut(2) {
                chunk.swap(0, 1);
            }
        }
        RomFormat::LittleEndian => {
            for chunk in data.chunks_exact_mut(4) {
                chunk.swap(0, 3);
                chunk.swap(1, 2);
            }
        }
    }
}

pub fn parse_header(data: &[u8], format: RomFormat) -> Result<RomHeader, RomError> {
    // 0x00: PI BSD DOM1 config (contains magic bytes — skip)
    let clock_rate = u32::from_be_bytes(data[0x04..0x08].try_into().unwrap());
    let entry_point = u32::from_be_bytes(data[0x08..0x0C].try_into().unwrap());
    let release = u32::from_be_bytes(data[0x0C..0x10].try_into().unwrap());
    let crc1 = u32::from_be_bytes(data[0x10..0x14].try_into().unwrap());
    let crc2 = u32::from_be_bytes(data[0x14..0x18].try_into().unwrap());
    let name_bytes = &data[0x20..0x34];
    let name = String::from_utf8_lossy(name_bytes)
        .trim_end_matches('\0')
        .trim()
        .to_string();
    let game_code: [u8; 4] = data[0x3B..0x3F].try_into().unwrap();
    let version = data[0x3F];

    Ok(RomHeader {
        format,
        clock_rate,
        entry_point,
        release,
        crc1,
        crc2,
        name,
        game_code,
        version,
    })
}
