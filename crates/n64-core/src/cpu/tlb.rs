/// TLB — Translation Lookaside Buffer.
///
/// 32-entry fully associative TLB that maps virtual addresses to physical.
/// Each entry maps a *pair* of pages (even + odd), with variable page sizes
/// from 4KB to 16MB controlled by PageMask.
///
/// Entry format:
///   PageMask[24:13] — mask bits (determines page size)
///   EntryHi[39:13]  — VPN2 (virtual page number / 2)
///   EntryHi[7:0]    — ASID (address space identifier)
///   EntryLo0/1      — even/odd page mappings:
///     [29:6] PFN (physical frame number, in 4KB units)
///     [2]    D (dirty/writable)
///     [1]    V (valid)
///     [0]    G (global — ignore ASID if BOTH Lo0 and Lo1 have G=1)

#[derive(Clone, Copy, Default)]
pub struct TlbEntry {
    pub page_mask: u32,
    pub entry_hi: u64,
    pub entry_lo0: u64,
    pub entry_lo1: u64,
}

pub struct Tlb {
    pub entries: [TlbEntry; 32],
}

impl Tlb {
    pub fn new() -> Self {
        Self {
            entries: [TlbEntry::default(); 32],
        }
    }

    /// Look up a virtual address in the TLB.
    /// Returns Some(physical_addr) on hit, None on miss.
    ///
    /// TODO(human): Implement this function.
    ///
    /// Algorithm — for each of the 32 TLB entries:
    ///
    /// 1. Compute the effective mask:
    ///      mask = (entry.page_mask as u64) | 0x1FFF
    ///    This gives the full offset range (4KB minimum).
    ///    The inverse (~mask) selects the VPN bits to compare.
    ///
    /// 2. Compare VPN: does the upper portion of vaddr match entry_hi?
    ///      (vaddr & !mask) == (entry.entry_hi & !mask)
    ///    If not, skip to next entry.
    ///
    /// 3. Check access permission:
    ///    - Global bit: G = (entry_lo0 bit 0) AND (entry_lo1 bit 0)
    ///    - If G=1, any ASID matches. If G=0, entry_hi[7:0] must == asid.
    ///
    /// 4. Determine even/odd page within the pair:
    ///      odd = (vaddr & ((mask + 1) >> 1)) != 0
    ///    Select entry_lo0 (even) or entry_lo1 (odd).
    ///
    /// 5. Check Valid bit (bit 1 of the selected entry_lo).
    ///    If not valid, return None (TLB Invalid).
    ///
    /// 6. Extract PFN and build physical address:
    ///      pfn = (entry_lo >> 6) & 0x00FF_FFFF   (24-bit frame number)
    ///      offset_mask = (mask >> 1) as u32       (per-page offset)
    ///      physical = (pfn << 12) | (vaddr & offset_mask)
    ///
    /// If no entry matches, return None (TLB Miss).
    pub fn lookup(&self, vaddr: u64, asid: u8) -> Option<u32> {
        for entry in &self.entries {
            let mask = entry.page_mask as u64 | 0x1FFF;
            if (vaddr & !mask) != (entry.entry_hi & !mask) {
                continue; // VPN mismatch
            }
            let global_bit = (entry.entry_lo0 & entry.entry_lo1 & 0x1) == 1;
            if !global_bit && (entry.entry_hi as u8 & 0xFF) != asid {
                continue; // ASID mismatch
            }
            let odd = (vaddr & ((mask + 1) >> 1)) != 0;
            let entry_lo = if odd {
                entry.entry_lo1
            } else {
                entry.entry_lo0
            };
            if (entry_lo & 0x2) == 0 {
                return None; // Not valid
            }
            let pfn = (entry_lo >> 6) & 0x00FF_FFFF;
            let offset_mask = (mask >> 1) as u32;
            let physical = (pfn << 12) | (vaddr & offset_mask as u64);
            return Some(physical as u32);
        }
        None
    }
}
