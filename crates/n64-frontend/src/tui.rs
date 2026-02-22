use std::io::stdout;
use std::time::Duration;

use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind},
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{
    prelude::*,
    widgets::{Block, Borders, Paragraph, Wrap},
};

use n64_core::cpu::cop0::Cop0;

const REG_NAMES: [&str; 32] = [
    "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3", "t0", "t1", "t2", "t3", "t4", "t5", "t6",
    "t7", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "t8", "t9", "k0", "k1", "gp", "sp", "fp",
    "ra",
];

pub fn run(mut n64: n64_core::N64) {
    enable_raw_mode().expect("enable raw mode");
    stdout()
        .execute(EnterAlternateScreen)
        .expect("enter alt screen");

    let backend = CrosstermBackend::new(stdout());
    let mut terminal = Terminal::new(backend).expect("create terminal");

    let mut paused = true; // start paused in debug mode
    let mut frame_count: u64 = 0;

    loop {
        if !paused {
            n64.run_frame();
            frame_count += 1;
        }

        terminal
            .draw(|f| draw_ui(f, &n64, paused, frame_count))
            .expect("draw");

        let poll_time = if paused {
            Duration::from_millis(50)
        } else {
            Duration::ZERO
        };
        if event::poll(poll_time).unwrap_or(false) {
            if let Ok(Event::Key(key)) = event::read() {
                if key.kind == KeyEventKind::Press {
                    match key.code {
                        KeyCode::Char('q') => break,
                        KeyCode::Char(' ') => paused = !paused,
                        KeyCode::Char('s') if paused => {
                            n64.run_frame();
                            frame_count += 1;
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    disable_raw_mode().expect("disable raw mode");
    stdout()
        .execute(LeaveAlternateScreen)
        .expect("leave alt screen");
}

fn draw_ui(f: &mut Frame, n64: &n64_core::N64, paused: bool, frame_count: u64) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(1)])
        .split(f.area());

    let main = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(55), Constraint::Percentage(45)])
        .split(chunks[0]);

    // Left: CPU registers
    let cpu_text = format_cpu_registers(n64);
    let cpu_widget = Paragraph::new(cpu_text)
        .block(Block::default().title(" CPU ").borders(Borders::ALL))
        .wrap(Wrap { trim: false });
    f.render_widget(cpu_widget, main[0]);

    // Right: system + video split
    let right = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)])
        .split(main[1]);

    let sys_text = format_system_status(n64);
    let sys_widget = Paragraph::new(sys_text)
        .block(Block::default().title(" System ").borders(Borders::ALL))
        .wrap(Wrap { trim: false });
    f.render_widget(sys_widget, right[0]);

    let vi_text = format_vi_status(n64);
    let vi_widget = Paragraph::new(vi_text)
        .block(Block::default().title(" Video ").borders(Borders::ALL))
        .wrap(Wrap { trim: false });
    f.render_widget(vi_widget, right[1]);

    // Status bar
    let status = if paused { "PAUSED" } else { "RUNNING" };
    let bar = Paragraph::new(format!(
        " {} | Frame {} | [Space] Pause  [S] Step  [Q] Quit",
        status, frame_count
    ))
    .style(Style::default().bg(Color::DarkGray).fg(Color::White));
    f.render_widget(bar, chunks[1]);
}

fn format_cpu_registers(n64: &n64_core::N64) -> String {
    let cpu = &n64.cpu;
    let mut s = format!(" PC  {:#018X}\n", cpu.pc);
    s += &format!(" HI  {:#018X}  LO  {:#018X}\n", cpu.hi, cpu.lo);
    s += &format!(
        " Delay slot: {}  LL bit: {}\n\n",
        cpu.in_delay_slot, cpu.ll_bit
    );

    for i in (0..32).step_by(2) {
        s += &format!(
            " {:4} {:#018X}  {:4} {:#018X}\n",
            REG_NAMES[i],
            cpu.gpr[i],
            REG_NAMES[i + 1],
            cpu.gpr[i + 1],
        );
    }
    s
}

/// Decode COP0 Status, Cause, and MI interrupt state into a human-readable
/// debug summary. This is the key "what is the system doing right now?" view.
fn format_system_status(n64: &n64_core::N64) -> String {
    let cop0 = &n64.cpu.cop0;
    let mi = &n64.bus.mi;

    let status = cop0.regs[Cop0::STATUS] as u32;
    let cause = cop0.regs[Cop0::CAUSE] as u32;

    // Status flags
    let ie = status & 1 != 0;
    let exl = status & 2 != 0;
    let erl = status & 4 != 0;
    let cu0 = status & (1 << 28) != 0;
    let cu1 = status & (1 << 29) != 0;

    let mut s = format!(
        " Status: IE:{} EXL:{} ERL:{} CU0:{} CU1:{}\n",
        ie as u8, exl as u8, erl as u8, cu0 as u8, cu1 as u8
    );

    // Effective interrupt state
    let irq_enabled = ie && !exl && !erl;
    s += &format!(
        " IRQs:   {}\n",
        if irq_enabled { "ENABLED" } else { "BLOCKED" }
    );

    // Cause: exception code
    let exc_code = (cause >> 2) & 0x1F;
    let exc_name = match exc_code {
        0 => "Int",
        1 => "TLB Mod",
        2 => "TLB Load",
        3 => "TLB Store",
        4 => "AddrLoad",
        5 => "AddrStore",
        8 => "Syscall",
        9 => "Break",
        10 => "Reserved",
        12 => "Overflow",
        13 => "Trap",
        15 => "FPE",
        _ => "Other",
    };
    let bd = cause & (1 << 31) != 0;
    s += &format!(
        " ExcCode: {} ({}){}\n",
        exc_code,
        exc_name,
        if bd { " [BD]" } else { "" }
    );
    s += &format!(" EPC:     {:#018X}\n\n", cop0.regs[Cop0::EPC]);

    // Interrupt Pending (Cause) vs Interrupt Mask (Status)
    let ip = (cause >> 8) & 0xFF;
    let im = (status >> 8) & 0xFF;
    s += &format!(" IP: {:08b}  IM: {:08b}\n", ip, im);
    s += &format!(" IP&IM:          {:08b}\n\n", ip & im);

    // MI interrupt sources (active / masked)
    const MI_NAMES: [&str; 6] = ["SP", "SI", "AI", "VI", "PI", "DP"];
    let mut active = Vec::new();
    let mut masked = Vec::new();
    for i in 0..6u8 {
        if mi.intr & (1 << i) != 0 {
            active.push(MI_NAMES[i as usize]);
        }
        if mi.intr_mask & (1 << i) != 0 {
            masked.push(MI_NAMES[i as usize]);
        }
    }
    let active_str = if active.is_empty() {
        "none".into()
    } else {
        active.join(" ")
    };
    let masked_str = if masked.is_empty() {
        "none".into()
    } else {
        masked.join(" ")
    };
    s += &format!(" MI active: {}\n", active_str);
    s += &format!(" MI mask:   {}\n\n", masked_str);

    // Timer
    s += &format!(
        " Count: {:#010X}  Compare: {:#010X}\n",
        cop0.regs[Cop0::COUNT] as u32,
        cop0.regs[Cop0::COMPARE] as u32,
    );

    s
}

fn format_vi_status(n64: &n64_core::N64) -> String {
    let vi = &n64.bus.vi;
    let fmt_name = match vi.ctrl & 0x3 {
        0 => "Blank",
        2 => "16-bit (5551)",
        3 => "32-bit (8888)",
        _ => "Reserved",
    };

    let mut s = format!(" Format:   {}\n", fmt_name);
    s += &format!(" Origin:   {:#010X}\n", vi.origin);
    s += &format!(" Width:    {} px\n", vi.width);
    s += &format!(" Scanline: {} / {}\n", vi.v_current, vi.v_sync);
    s += &format!(" V_INTR:   line {}\n", vi.v_intr);
    s += &format!(" X scale:  {:#010X}\n", vi.x_scale);
    s += &format!(" Y scale:  {:#010X}\n", vi.y_scale);
    s
}
