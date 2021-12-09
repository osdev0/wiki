# Stivale 2 basics
This is a simple intro to OS development using the Stivale 2 boot protocol.  
This is meant to help as a quick intro to osdev, and is not a full guide.  

For this tutotial, you need a x86_64-elf [cross compiler](./wiki/gcc_cross.md).  
## The build
Download `stivale2.h` [here](https://raw.githubusercontent.com/stivale/stivale/master/stivale2.h)
### kernel.c
```c
#include <stdint.h>
#include <stddef.h>
#include "stivale2.h"

static uint8_t stack[8192];
static struct stivale2_header_tag_terminal terminal_hdr_tag = {
    .tag = {
        .identifier = STIVALE2_HEADER_TAG_TERMINAL_ID,
        .next = 0
    },
    .flags = 0
};
static struct stivale2_header_tag_framebuffer framebuffer_hdr_tag = {
    .tag = {
        .identifier = STIVALE2_HEADER_TAG_FRAMEBUFFER_ID,
        .next = (uint64_t)&terminal_hdr_tag
    },
    .framebuffer_width  = 0,
    .framebuffer_height = 0,
    .framebuffer_bpp    = 0
};
__attribute__((section(".stivale2hdr"), used))
static struct stivale2_header stivale_hdr =
{
    .entry_point = 0,
    .stack = (uintptr_t)stack + sizeof(stack),
    .flags = (1 << 1) | (1 << 2) | (1 << 3) | (1 << 4),
    .tags = (uintptr_t)&framebuffer_hdr_tag
};
void *stivale2_get_tag(struct stivale2_struct *stivale2_struct, uint64_t id) {
    struct stivale2_tag *current_tag = (void *)stivale2_struct->tags;
    for(;;)
    {
        if(current_tag == NULL) return NULL;
        if (current_tag->identifier == id) return current_tag;
        current_tag = (void *)current_tag->next;
    }
}

void *gop_addr; // Pointer to GOP
void write(const char* in) 
{
    void (*gop_write)(const char *string, size_t length) = gop_addr;
    size_t len = 0;
	while(in[len]) len++;
    gop_write(in, len);
}

void _start(struct stivale2_struct *stivale2_struct) {
    struct stivale2_struct_tag_terminal *term_str_tag = stivale2_get_tag(stivale2_struct, STIVALE2_STRUCT_TAG_TERMINAL_ID);
    if(term_str_tag == NULL) for(;;) asm ("hlt");
    gop_addr = (void *)term_str_tag->term_write;

    write("Hello, World!");
 
    // hang
    for(;;) asm ("hlt");
}
```
Compile this with "`x86_64-elf-gcc -c kernel.c -o kernel.o -ffreestanding -mno-red-zone -mcmodel=kernel -Wall -Wextra -O2`".
### linker.ld
```linker
OUTPUT_FORMAT(elf64-x86-64)
OUTPUT_ARCH(i386:x86-64)
ENTRY(_start)
PHDRS
{
    null    PT_NULL    FLAGS(0) ;                   /* Null segment */
    text    PT_LOAD    FLAGS((1 << 0) | (1 << 2)) ; /* Execute + Read */
    rodata  PT_LOAD    FLAGS((1 << 2)) ;            /* Read only */
    data    PT_LOAD    FLAGS((1 << 1) | (1 << 2)) ; /* Write + Read */
}
SECTIONS
{
    . = 0xffffffff80000000; 
    .text : {
        *(.text*)
    } :text
    . += CONSTANT(MAXPAGESIZE);
    .stivale2hdr : {
        KEEP(*(.stivale2hdr))
    } :rodata
    .rodata : {
        *(.rodata*)
    } :rodata
    . += CONSTANT(MAXPAGESIZE);
    .data : {
        *(.data*)
    } :data
    .bss : {
        *(COMMON)
        *(.bss*)
    } :data
}
```
Link using "`x86_64-elf-ld kernel.o -o kernel.elf -Tlinker.ld -nostdlib`".  
And you're done with the kernel! But how will it boot?
## Making an ISO
First, download the Limine bootloader with "`git clone https://github.com/limine-bootloader/limine.git --branch=v2.0-branch-binary --depth=1`".  
Then run this command to generate a .ISO file
```
make -C limine
mkdir -p iso_root
cp -v kernel.elf limine.cfg limine/limine.sys limine/limine-cd.bin limine/limine-eltorito-efi.bin iso_root/
xorriso -as mkisofs -b limine-cd.bin -no-emul-boot -boot-load-size 4 -boot-info-table --efi-boot limine-eltorito-efi.bin -efi-boot-part --efi-boot-image --protective-msdos-label iso_root -o image.iso
./limine/limine-install image.iso
```
