<hr/>

<p align="center">
<a href="./modules/ui/doom-dashboard/banners/default.png"><img src="./modules/ui/doom-dashboard/banners/default.png" alt="wing cat"></a>
</p>

# Memacs

## Usage

### 1. Clone From Github

```shell
git clone https://github.com/MephistoMMM/memacs.git --single-branch --depth=1
```

### 2. Backup Your Origin Emacs Config

```shell
mv ~/.emacs.d ~/.bak.emacs.d
# if you have latexmkrc file under your home
mv ~/.latexmkrc ~/.bak.latexmkrc
```

### 2.5. Install Emacs

MacOS :

```shell
./scripts/install_emacs_mac.sh
```

Ubuntu:

```shell
./scripts/install_emacs_ubuntu.sh
```

### 3. Run Link Script

```shell
./scripts/link_memacs.sh
```

## License

[GPLv3](./LICENSE)
