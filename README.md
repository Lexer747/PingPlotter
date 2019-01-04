# Did you always want a haskell & command line based ping graphing tool?

Of course you didn't. So I wrote one.

## Installation

1. Have the haskell platform installed. (> 8.0)

2. Run these commands for Linux:

``` shell
$ git clone https://github.com/Lexer747/PingPlotter.git
$ cd /PingPlotter/
$ ./install.sh
```
Or for windows:

``` shell
> git clone https://github.com/Lexer747/PingPlotter.git
> cd .\PingPlotter\
> .\install.bat
```

Or for MacOS:

``` shell
$ echo "lol i don't have mac to test it on, gl"
```

##### Note, these commands take a while on either system to complete

3. Then call the exe with the website you want to test the ping too:

``` shell
$ ./Ping-v2-0-0 "www.google.com"
```

4. Use at your own risk!

### Demo

![](samples/HostnameDemo.gif)

<sub> Yes, I know, my ping sucks. </sub>

## Feature List

* Draws a graph showing the ping of a website or IP address
* Use the `-h` option for a list of options while running
* View a previously plotted ping in its entirety. Stored in the `.ping` file.
* other list item
* also another list item

## Un-installation

Since you used it once you'll need this command.

``` shell
$ yes | rm -r PingPlotter/
```

### Credits & License

Some dude - ![LICENSE](LICENSE)
