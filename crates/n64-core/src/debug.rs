/// Debug infrastructure for the N64 emulator.
///
/// `DebugState` accumulates per-frame data from the renderer and GBI walker.
/// The frontend reads it to draw overlays. Zero-cost when disabled — all
/// recording paths are guarded by bool checks.

/// A recorded triangle edge for wireframe overlay.
#[derive(Clone, Copy)]
pub struct WireEdge {
    pub x0: f32,
    pub y0: f32,
    pub x1: f32,
    pub y1: f32,
}

/// A single GBI command log entry.
#[derive(Clone)]
pub struct DlLogEntry {
    pub pc: u32,
    pub w0: u32,
    pub w1: u32,
    pub opcode_name: &'static str,
}

/// Per-frame statistics snapshot.
#[derive(Clone, Default)]
pub struct FrameStats {
    pub tri_count: u32,
    pub vtx_count: u32,
    pub fill_rect_count: u32,
    pub tex_rect_count: u32,
    pub dl_cmd_count: u32,
    pub rsp_gfx_tasks: u32,
    pub rsp_audio_tasks: u32,
    pub culled_back: u32,
    pub culled_front: u32,
    pub near_clip_count: u32,
}

/// Configuration flags — what debug features are active.
#[derive(Clone, Default)]
pub struct DebugFlags {
    pub show_stats: bool,
    pub show_wireframe: bool,
    pub show_depth: bool,
    pub show_textures: bool,
    pub show_geometry: bool,
    pub show_dl_log: bool,
    pub show_os_monitor: bool,
    pub freeze_frame: bool,
}

/// Ring buffer for display list log entries.
pub struct DlLog {
    entries: Vec<DlLogEntry>,
    capacity: usize,
    write_pos: usize,
}

impl DlLog {
    pub fn new(capacity: usize) -> Self {
        Self {
            entries: Vec::with_capacity(capacity.min(256)),
            capacity,
            write_pos: 0,
        }
    }

    pub fn push(&mut self, entry: DlLogEntry) {
        if self.entries.len() < self.capacity {
            self.entries.push(entry);
        } else {
            self.entries[self.write_pos] = entry;
        }
        self.write_pos = (self.write_pos + 1) % self.capacity;
    }

    /// Iterate the most recent `count` entries in chronological order.
    pub fn iter_recent(&self, count: usize) -> impl Iterator<Item = &DlLogEntry> {
        let len = self.entries.len();
        let count = count.min(len);
        let start = if len < self.capacity {
            len.saturating_sub(count)
        } else {
            (self.write_pos + self.capacity - count) % self.capacity
        };
        (0..count).map(move |i| &self.entries[(start + i) % len])
    }
}

/// Cached OS thread info for the monitor overlay.
#[derive(Clone)]
pub struct ThreadInfo {
    pub vaddr: u32,
    pub priority: u32,
    pub state: u16,
    pub id: u32,
    pub saved_pc: u32,
}

/// The main debug state container.
pub struct DebugState {
    pub flags: DebugFlags,
    pub stats: FrameStats,
    pub prev_stats: FrameStats,
    pub wire_edges: Vec<WireEdge>,
    pub dl_log: DlLog,
    pub frame_count: u64,
    pub fps_samples: [f64; 60],
    pub fps_write_pos: usize,
    /// Cached thread list for OS monitor (rescanned periodically).
    pub cached_threads: Vec<ThreadInfo>,
    pub os_scan_countdown: u32,
}

impl DebugState {
    pub fn new() -> Self {
        Self {
            flags: DebugFlags::default(),
            stats: FrameStats::default(),
            prev_stats: FrameStats::default(),
            wire_edges: Vec::new(),
            dl_log: DlLog::new(4096),
            frame_count: 0,
            fps_samples: [16.666; 60],
            fps_write_pos: 0,
            cached_threads: Vec::new(),
            os_scan_countdown: 0,
        }
    }

    /// Call at the start of each frame to rotate stats and clear per-frame buffers.
    pub fn begin_frame(&mut self) {
        if !self.flags.freeze_frame {
            self.prev_stats = self.stats.clone();
            self.stats = FrameStats::default();
            self.wire_edges.clear();
        }
    }
}
