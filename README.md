# Vibe64

A fully vibe coded n64 emulator.

This project has both an interpreter and a tiered, async tracing dynamic recompiler JIT with multiple levels of optimization via a Cranelift backend.

Zelda OoT works great -- it's fully functional, just some minor bugs.
Mario 64 is playable. Many other games likely are too.

Given that the models used almost certainly trained on GPL data around n64 emulation, including Project64 and Mupen and likely they also used web search to directly or indirectly read their code at various points, it only makes sense for this project to also be considered GPL. If you wanted to vibe code an N64 emulator without that, you'd have to work pretty hard to make sure it was independent and frankly I don't know if you even could if that code ended up in the training data. Anyway, this isn't a serious project (though it does work quite well), this is just for fun and something I made to learn about Agentic coding more.

I also made this mostly from my phone.
