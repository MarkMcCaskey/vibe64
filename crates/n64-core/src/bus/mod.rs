pub mod map;

/// Memory bus trait. All memory access goes through this.
///
/// The CPU calls read/write on the Bus; the Bus dispatches to
/// the correct hardware component based on physical address.
///
/// We use u32 for physical addresses because the N64's physical
/// address space is 32-bit. The 64-bit virtual addresses are resolved
/// by the CPU before hitting the bus.
pub trait Bus {
    fn read_u8(&self, addr: u32) -> u8;
    fn read_u16(&self, addr: u32) -> u16;
    fn read_u32(&self, addr: u32) -> u32;
    fn read_u64(&self, addr: u32) -> u64;

    fn write_u8(&mut self, addr: u32, val: u8);
    fn write_u16(&mut self, addr: u32, val: u16);
    fn write_u32(&mut self, addr: u32, val: u32);
    fn write_u64(&mut self, addr: u32, val: u64);

    /// Called when DMA writes to RDRAM. JIT listens here to
    /// invalidate compiled blocks covering the written range.
    fn notify_dma_write(&mut self, start: u32, len: u32);

    /// Check for pending unmasked interrupts (MI_INTR & MI_MASK).
    fn pending_interrupts(&self) -> bool;
}
