Assembler Opcodes
=================

**lookup  name, <target>**

look up 'name' in the current environment and store the address
in register 'target'

**environment address**

Load the current environment pointer to point to address

**moverr <register>, <register>**
**movesr <special>, <register>**
**movers <register>, <special>**

**moveimm <register>, immediate, size**
**moveimm immediate, <register>, size**

**addimm <register>, immediate**
**addrr  <register>, <register>**

**jump cond, <register>**

**jumprel cond, immediate**

**shift <register>, immediate**
**ior <register>, immediate**
**and <register>, immediate**
**xor <register>, immediate**

**push <register>**
**pushi immediate**
**pop <register>**
**pushs <special>**
**pops <special>**

**call <register>,style**
**ret style**
