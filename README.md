# emacs
My personal emacs configuration

It is mostly based on tuhdo's emacs ide demo: https://github.com/tuhdo/emacs-c-ide-demo

This configuration requires the installation of :
 - the GNU global package (for gtags)
 - clang (for ivory)
 - cmake (for ivory)
 - llvm-libs (for cmake, somehow not a dependency on Manjaro when installing cmake)
 - Use python-pip to install jedi, flake8, importmagic and autopep8 (for elpy)

When first checking out this config, run irony-install-server to make and install the irony-server.
