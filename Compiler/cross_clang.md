# clang Cross-Compiler

## Introduction

A [cross compiler](https://en.wikipedia.org/wiki/Cross_compiler) is a compiler that runs on platform A (the **host**), but generates executable code for platform B (the **target**). The platforms do not need to but usually differ in CPU architecture, library environment, ABI, and/or executable format. For operating system development, the host is our development system, and the target is the developed OS. It is important to realize that these two platforms are not the same: the developed OS will always differ from the development OS. Thus it is necessary to use a cross compiler to avoid various troubles down the road.

## Why cross compilation is necessary

Unless development is being done on your developed OS, a cross compiler is necessary. The compiler must know the correct target platform, otherwise there will be issues, if not now then later. If the system compiler is used, it will not know that it is not targeting your host OS: it compiling something else entirely. Some tutorials suggest using the system compiler, and passing it several options to tell it that it's not targetting the host. This is not sustainable in the long run and the real solution is to use a cross-compiler. Further argumentation for using a cross compiler, if this wasn't convincing enough, can be found in the article [Why do I need a cross compiler?](Cross_Compiler.md)

## The clang compiler

As an operating system developer in the 2020s, [newer technologies](../Modern/time_travel.md) have been developed that not only simplify the development process, but are of higher quality and are more powerful than what was used before. One of these technologies is [LLVM](https://llvm.org/). Basically all osdev guides, including the original osdev.org wiki instruct the developer to use GCC. Using LLVM+clang makes the life of an OS developer easier. Unlike the horribly dated GNU Compiler Collection, this compiler was designed as a cross-compiler in its [inception](https://en.wikipedia.org/wiki/LLVM), and thus it is not neccessary to build an entire toolchain from scratch for each desired target platform. There are several other benefits to using clang over GCC, but for OS developers, this is the main one. It is the industry-standard cross-platform compiler: using clang allows OS development to take place on Windows without requiring the use of emulation/compatibility layers like MSYS or Cygwin, or locking down the build system to MSVC. It is also the native compiler on BSD and Mac OS X systems. The error messages are more accurate and informational. See the [clang](../Compilers/clang.md) page for more on why OS development should be done using LLVM.

### Targets
Cross compilation is done using a [target triplet](Target_Triplet.md), which conveys information to the compiler about the platform that is being compiled to.

## Start

Install clang on the host system. It is the standard compiler on BSD-like systems, and virtually all Linux distributions have clang packaged in their software repositories. There is an installer for Windows.

### Linux

* Ubuntu-based: `# apt install clang`
* Arch-based: `# pacman -S clang`
* Fedora: `# dnf install clang`
* Gentoo: `# emerge -a clang`
  - Be sure to enable the target architecture(s) in the LLVM_TARGETS variable in /etc/portage/make.conf
* Alpine: `# pkg add clang`
* Others: It is highly unlikely the distribution does not provide clang packaged. Search the repositories. In the case that clang isn't packaged, you can build it from source.

### Windows
Install the latest release from the [LLVM GitHub release page](https://github.com/llvm/llvm-project/releases).

TODO: Windows

### *BSD

Clang is the default compiler for OpenBSD, NetBSD and FreeBSD so it should be preinstalled

### macOS

The built-in apple clang comes crippled: it only ships with x86_64 and arm64 support (but still supports ELF). It is recommended to get clang from [homebrew](https://brew.sh/), as the LLVM linker (lld) is needed to get anything meaningful done either way. It is also worth noting that proper cross-compilation GCC tools are also in the homebrew repositories (under the names x86_64-elf-binutils and x86_64-elf-gcc).

## Enabling cross-compilation

TODO
