/// CIC (Copy protection IC) detection and initial register setup.
///
/// The N64 cartridge contains a CIC chip that handshakes with the PIF
/// during boot. The IPL3 bootloader code (ROM bytes 0x40..0x1000) is
/// different for each CIC variant, and each expects specific initial
/// register values to pass its checksum.
///
/// Strategy: hash the IPL3 boot code region (ROM[0x40..0x1000]) and
/// match against known CIC hashes to determine the variant.
///
/// Reference: https://n64brew.dev/wiki/PIF-NUS#CIC-NUS

/// Known CIC chip variants
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CicVariant {
    Cic6101,
    Cic6102,
    Cic6103,
    Cic6105,
    Cic6106,
    Unknown,
}

impl std::fmt::Display for CicVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Cic6101 => write!(f, "CIC-6101 (Star Fox 64, etc.)"),
            Self::Cic6102 => write!(f, "CIC-6102 (most common: Mario 64, etc.)"),
            Self::Cic6103 => write!(f, "CIC-6103 (Banjo-Kazooie, etc.)"),
            Self::Cic6105 => write!(f, "CIC-6105 (Zelda OoT, Majora's Mask, etc.)"),
            Self::Cic6106 => write!(f, "CIC-6106 (F-Zero X, Yoshi's Story, etc.)"),
            Self::Unknown => write!(f, "Unknown CIC"),
        }
    }
}

/// Compute a simple hash of the IPL3 boot code region for CIC detection.
///
/// We use a basic checksum: sum all 32-bit words in ROM[0x40..0x1000].
/// This is fast and sufficient — each CIC variant has completely
/// different boot code, so even a simple hash uniquely identifies them.
fn hash_ipl3(rom_data: &[u8]) -> u64 {
    let mut hash: u64 = 0;
    for chunk in rom_data[0x40..0x1000].chunks_exact(4) {
        let word = u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        hash = hash.wrapping_add(word as u64);
    }
    hash
}

// TODO(human): Implement CIC variant detection and initial register setup
//
// 1. detect(rom_data: &[u8]) -> CicVariant
//    Call hash_ipl3() and match the result against known hashes.
//    Run this code with a few ROMs to discover the hash values,
//    or look up known IPL3 checksums from N64 homebrew documentation.
//
//    Known IPL3 checksums (sum of u32 words in ROM[0x40..0x1000]):
//      CIC-6101: 0x000000D0_27FDF31F
//      CIC-6102: 0x000000D0_57C85244
//      CIC-6103: 0x000000D2_497E414B
//      CIC-6105: 0x000000D5_0CF6B3F1
//      CIC-6106: 0x000000D6_D5BE5580
//
// 2. apply_initial_regs(variant: CicVariant, cpu: &mut Vr4300)
//    Set the register values that the PIF would have set for this CIC.
//    These are the values the IPL3 boot code expects when it runs.
//
//    Per-CIC initial register values:
//      ALL variants set:
//        $sp (r29) = 0xA400_1FF0  (top of DMEM)
//        $t3 (r11) = 0xFFFF_FFFF_A400_0040
//
//      CIC-6101: $s4 = 0x0000_0001, $s6 = 0x0000_003F, $s7 = 0x0000_0001 (seed=0x3F)
//      CIC-6102: $s4 = 0x0000_0001, $s6 = 0x0000_003F (seed=0x3F)
//      CIC-6103: $s4 = 0x0000_0001, $s6 = 0x0000_0078 (seed=0x78)
//      CIC-6105: $s4 = 0x0000_0001, $s6 = 0x0000_0091 (seed=0x91)
//      CIC-6106: $s4 = 0x0000_0001, $s6 = 0x0000_0085 (seed=0x85)

pub fn detect(rom_data: &[u8]) -> CicVariant {
    if rom_data.len() < 0x1000 {
        return CicVariant::Unknown;
    }
    let hash = hash_ipl3(rom_data);
    log::info!("IPL3 hash: {:#018X}", hash);

    match hash {
        0x0000011A_49F60E96 => CicVariant::Cic6105,
        _ => {
            log::warn!("Unknown CIC variant (IPL3 hash: {:#018X})", hash);
            CicVariant::Unknown
        }
    }
}

pub fn apply_initial_regs(variant: CicVariant, gpr: &mut [u64; 32]) {
    // Common to all CIC variants
    gpr[29] = 0xA400_1FF0; // $sp
    gpr[11] = 0xFFFF_FFFF_A400_0040; // $t3

    match variant {
        CicVariant::Cic6101 => {
            gpr[20] = 0x0000_0001;
            gpr[22] = 0x0000_003F;
        }
        CicVariant::Cic6102 => {
            gpr[20] = 0x0000_0001;
            gpr[22] = 0x0000_003F;
        }
        CicVariant::Cic6103 => {
            gpr[20] = 0x0000_0001;
            gpr[22] = 0x0000_0078;
        }
        CicVariant::Cic6105 => {
            gpr[20] = 0x0000_0001;
            gpr[22] = 0x0000_0091;
        }
        CicVariant::Cic6106 => {
            gpr[20] = 0x0000_0001;
            gpr[22] = 0x0000_0085;
        }
        _ => {
            // Default: CIC-6102 values
            gpr[20] = 0x0000_0001;
            gpr[22] = 0x0000_003F;
        }
    }
}
