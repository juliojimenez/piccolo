# piccolo

piccolo is a simple CLI Gopher client written in Common Lisp.

## Installation

Download a [release](https://github.com/juliojimenez/piccolo/releases) (recommended)

```bash
wget https://github.com/juliojimenez/piccolo/archive/refs/tags/v0.0.3.tar.gz -O piccolo.tar.gz
tar -xvzf piccolo.tar.gz
cd piccolo-0.0.3
```

Clone the repository (may be unstable)

```bash
git clone https://github.com/juliojimenez/piccolo
cd piccolo
```

piccolo requires QuickLisp in order to install dependencies. You can check by using `make ql-check`.

```bash
make ql-check
Quicklisp is missing. Run make ql-install.
```

To install QuickLisp, run `make ql-install`. Then run `make ql-check` again to verify the installation.

```bash
make ql-check 
Quicklisp is installed.
```

Now let's create a binary we can use by running `make build`

## Burrowing The Gopherspace

Start piccolo with or without a `gopher://` URI.

```bash
./piccolo
./piccolo gopher://gopher.quux.org
```

If no URI is provided, piccolo will start at `gopher://gopher.quux.org`.

Each item associated with an item type has a line number. To navigate to an item, just input its line number into the prompt and press `[ENTER]`.

## Contributing

Please do! I don't know what I'm doing. 😅

## It works on my computer

I've tested piccolo on:

- macOS M1 15.3.2+, SBCL 2.4.8+
- Linux 4.16.18-galliumos, SBCL 1.4.5+

## Troubleshooting

- I can't see the item type icons/emojis (i.e. file, folder, etc.).

In Ubuntu, I was able to fix this with `apt get fonts-noto-color-emoji` and restarting the terminal.

On macOS, I use iTerm2 and have not encountered any issues with emoji support.

This [article](https://linuxjedi.co.uk/rendering-emoji-in-linux-terminals/) has more information on how to fix things in Fedora/CentOS.