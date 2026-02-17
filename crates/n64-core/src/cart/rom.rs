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

// TODO(human): Implement these three functions:
//
// 1. detect_format(magic: [u8; 4]) -> Result<RomFormat, RomError>
//    Match the magic bytes against the three known formats.
//
// 2. normalize(data: &mut Vec<u8>, format: RomFormat)
//    Byte-swap the ROM data in-place to big-endian (z64) format.
//    BigEndian: do nothing
//    ByteSwapped: swap every pair of bytes
//    LittleEndian: reverse every 4-byte word
//
// 3. parse_header(data: &[u8], format: RomFormat) -> Result<RomHeader, RomError>
//    Parse the 64-byte header. After normalization, data is big-endian.
//    Use u32::from_be_bytes() to read 32-bit fields.
//    Offsets: see doc/n64_architecture.md "ROM Header" section.

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

pub fn detect_format(_magic: [u8; 4]) -> Result<RomFormat, RomError> {
    todo!("Implement ROM format detection from magic bytes")
}

pub fn normalize(_data: &mut Vec<u8>, _format: RomFormat) {
    todo!("Implement byte-swap normalization to big-endian")
}

pub fn parse_header(_data: &[u8], _format: RomFormat) -> Result<RomHeader, RomError> {
    todo!("Implement ROM header parsing")
}
