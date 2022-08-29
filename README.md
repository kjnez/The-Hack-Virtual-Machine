# virtual-machine

This project is an implementation of a virtual machine to translate VM source code to assembly code in the Hack system. It follows the contract described in **The Elements of Computing Systems**.

## Usage
You can use it by providing VM source file and the output file as follows:

```lisp
(virtual-machine:write-file "test/StaticTest.vm" "StaticTest.asm")
```

## License

MIT

