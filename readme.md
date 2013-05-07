# Theme Park mode

So much to see in Theme Park!


With this minor mode enabled you can cycle through your installed themes. I
made it into a minor mode so that you're not limited to any particular buffer
or other constraints when you want to view themes.


Designed to be deactivated (C-c C-q / down arrow) once you're done deciding on
a theme.

## Installation

Drop `theme-park-mode.el` in your Emacs `load-path` and `(require 'theme-park-mode)`.

## Usage

    M-x theme-park-mode

    C-c C-n / right arrow = next theme
    C-c C-p / left arrow  = previous theme
    C-c C-r / up arrow    = start over
    C-c C-q / down arrow  = quit

## TODOs / Maybes / Perhapses

* At the moment previous theme is just that, the previous theme. Does not go backwards.
* Marmalade/MELPA?
* Clearly visible text of current theme, like an overlay box or something.
* Setting for group of regularly used themes to cycle between?

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

