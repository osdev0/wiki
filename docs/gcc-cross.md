# GCC Cross-Compiler

## Enviroment
### Requirements
`gcc`, `g++`, `make`, `bison`, `flex`, `gmp`, `mpc`, `mpfr`, and `texinfo` are needed to properly build GCC and BINUTILS.
### Constants
It is reccomended to set constants later used when configuring.  
`$PREFIX` holds the location to install to. This can be whatever you want (It is reccomended to build GCC and BINUTILS to the same directory).  
`$TARGET` is the target architecture. This can be any valid target.  
For example, [Multiboot 1 basics](./basics.md) would use:  
```bash
export PREFIX="$HOME/opt/cross"
export TARGET=i686-elf
export PATH="$PREFIX/bin:$PATH"
```
## The build
### Building BINUTILS
First, download BINUTILS from the GNU mirror:  
`git clone git://sourceware.org/git/binutils.git`  
You can extract it where ever you like, we just use `$HOME/src` because the original documentation used it.
```bash
cd $HOME/src
mkdir build-binutils
cd build-binutils
../binutils-*.*/configure --target=$TARGET --prefix="$PREFIX" --with-sysroot --disable-nls --disable-werror
make
make install
```
### Building GCC
Download GCC from the GNU mirror:  
`git clone git://sourceware.org/git/gcc.git`  
Again, extract it to your preferred location, we are using `$HOME/src`.  
If you don't wish to compile for C or C++, or need support for another language, change `--enable-languages` as needed.
```bash
cd $HOME/src

mkdir build-gcc
cd build-gcc
../gcc-*.*/configure --target=$TARGET --prefix="$PREFIX" --disable-nls --enable-languages=c,c++ --without-headers
make all-gcc
make all-target-libgcc
make install-gcc
make install-target-libgcc
```
### Common errors
* `No rule to make target 'install'` when compiling libgcc  
The exact cause is unknown, however may have to do with bad source copy or building GCC and BINUTILS to different directories
* libsanitizer fails to build  
If this error occors, apply `--disable-libsanitizer` when configuring.
