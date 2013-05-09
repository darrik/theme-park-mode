# Theme Park mode

So much to see in Theme Park!


With this minor mode enabled you can cycle through your installed themes. I
made it into a minor mode so that you're not limited to any particular buffer
or other constraints when you want to view themes.


Designed to be deactivated (C-c C-q / down arrow) once you're done deciding on
a theme.

## Installation

Available on [marmalade](http://marmalade-repo.org/) and you can install it
via `M-x package-install theme-park-mode` or manually by dropping
`theme-park-mode.el` in your Emacs `load-path` and `(require
'theme-park-mode)`.

## Usage

    M-x theme-park-mode

    C-c C-n / right arrow = next theme
    C-c C-p / left arrow  = previous theme
    C-c C-r / up arrow    = start over
    C-c C-q / down arrow  = quit
    C-c C-c               = show name of current theme
    C-c C-g               = toggle global / local mode
    C-c C-l               = toggle global / local mode
    C-c C-t               = tag theme for inclusion in local mode
    C-c C-d               = show tagged themes
    C-c C-s               = save tagged themes for future sessions

## Local mode

While browsing through themes with this minor mode you can "tag" themes for a
smaller list of frequently used themes. Use `C-c C-t` when on a theme you like
and it will be tagged. When you have two or more themes tagged you can go into
"local mode" with `C-c C-l` and cycle between them. You can save the list for
future sessions with `C-c C-s`.

### Configuration

You can use `M-x customize-group theme-park-mode` or set `tpm-tagged` in your
init file (i.e: `(setq tpm-tagged '(zenesque-dark-colors underwaterd))`) if
you want to change the list manually.

## Notes

Requires Emacs v24+

### Psst, look here

If you're experiencing flashing, it's beacuse I unload the current theme
before going to the next one, to avoid face tainting. So be careful if you're
sensitive to that.

## License

Copyright (C) 2013 Rikard Glans

Author: Rikard Glans <rikard@ecx.se>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

