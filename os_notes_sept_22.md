# Virtual Memory

Page tables are stored in kernel address space in the OS memory

4 kB Virtual Address
1 kB pages

  |VPN A| VPN B|
0:|     |      |
1:|     |      |
2:|     |      |
3:|     |   7  |

8kB of Physical Memory
Each block is a "page frame"
0:
1:
2:
3:
4:
5:
6:
7:

Page Table
Index through VPN (Virtual page number) get a PFN (Physical Frame Number)
VPN has status bits as well. In this case V is a valid bit.
  | VPN | V |PFN |
0:| 7   | 1 |    |
1:|     |   |    |
2:|     |   |    |
3:|     |   |    |

# Multi Level Page Tables

Turns the linear page table into something like a tree.
