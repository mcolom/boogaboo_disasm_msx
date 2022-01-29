# boogaboo_disasm_msx
This is a disassembly of the Z80 MSX-1 code of the MSX-1 "BOOGA-BOO" game.

This was the first game I played in my Sony HB-20P when I was a child, probably eight years old.

La Pulga is considered one of the first games made in Spain, and it's a very well known work by Francisco Suárez and Francisco Portalo ("Paco & Paco". Book here: http://www.bugabootheflea.com/). However, the MSX version was wrote by two persons about whom we known almost nothing nowadays. A complete mystery that absolutely puzzles me. They only think we know about the authors of the MSX version is the short notice that they showed in the game: "Quicksilva presents BOOGA-BOO. By Steve & Ann Haigh".

Discovering all the thoughts of its creators as I was deciphering each byte has simply been as amazing as addictive.

I hope you retro-lovers will enjoy it too :)

Miguel

=======================


The main file is `disassembly.asm`. Compile it with: `z80asm disassembly.asm`

It should generate a a.bin file with the following SHA sum: `9a63f2a6064a8e28f7251f78c3e91672f5de5253`.

If you want to create a CAS file, simply join the `HEADER` file together with `a.bin` and rename it to BOOGA.CAS.

![](http://mcolom.info/ext_images/pulga/gi_scenario.png)

