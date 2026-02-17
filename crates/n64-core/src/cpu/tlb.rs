/// TLB — Translation Lookaside Buffer.
///
/// 32-entry fully associative TLB with variable page sizes.
/// Implementation deferred to Phase 10.

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
    /// Returns Some(physical_addr) on hit, None on miss (TLB exception).
    pub fn lookup(&self, _vaddr: u64, _asid: u8) -> Option<u32> {
        // Stub — will implement in Phase 10
        None
    }
}
