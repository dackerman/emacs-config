;;; lsp-dart-flutter-colors.el --- Flutter color names -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Eric Dallo
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flutter colors

;;; Code:

(defconst lsp-dart-flutter-colors
  '(("transparent" . "000000")
    ("black" . "000000")
    ("black87" . "000000")
    ("black54" . "000000")
    ("black38" . "000000")
    ("black45" . "000000")
    ("black26" . "000000")
    ("black12" . "000000")
    ("white" . "ffffff")
    ("white70" . "ffffff")
    ("white30" . "ffffff")
    ("white12" . "ffffff")
    ("white10" . "ffffff")
    ("grey" . "9e9e9e")
    ("grey[50]" . "fafafa")
    ("grey[100]" . "f5f5f5")
    ("grey[200]" . "eeeeee")
    ("grey[300]" . "e0e0e0")
    ("grey[350]" . "d6d6d6")
    ("grey[400]" . "bdbdbd")
    ("grey[500]" . "9e9e9e")
    ("grey[600]" . "757575")
    ("grey[700]" . "616161")
    ("grey[800]" . "424242")
    ("grey[850]" . "303030")
    ("grey[900]" . "212121")
    ("red" . "f44336")
    ("red[50]" . "ffebee")
    ("red[100]" . "ffcdd2")
    ("red[200]" . "ef9a9a")
    ("red[300]" . "e57373")
    ("red[400]" . "ef5350")
    ("red[500]" . "f44336")
    ("red[600]" . "e53935")
    ("red[700]" . "d32f2f")
    ("red[800]" . "c62828")
    ("red[900]" . "b71c1c")
    ("pink" . "e91e63")
    ("pink[50]" . "fce4ec")
    ("pink[100]" . "f8bbd0")
    ("pink[200]" . "f48fb1")
    ("pink[300]" . "f06292")
    ("pink[400]" . "ec407a")
    ("pink[500]" . "e91e63")
    ("pink[600]" . "d81b60")
    ("pink[700]" . "c2185b")
    ("pink[800]" . "ad1457")
    ("pink[900]" . "880e4f")
    ("purple" . "9c27b0")
    ("purple[50]" . "f3e5f5")
    ("purple[100]" . "e1bee7")
    ("purple[200]" . "ce93d8")
    ("purple[300]" . "ba68c8")
    ("purple[400]" . "ab47bc")
    ("purple[500]" . "9c27b0")
    ("purple[600]" . "8e24aa")
    ("purple[700]" . "7b1fa2")
    ("purple[800]" . "6a1b9a")
    ("purple[900]" . "4a148c")
    ("deepPurple" . "673ab7")
    ("deepPurple[50]" . "ede7f6")
    ("deepPurple[100]" . "d1c4e9")
    ("deepPurple[200]" . "b39ddb")
    ("deepPurple[300]" . "9575cd")
    ("deepPurple[400]" . "7e57c2")
    ("deepPurple[500]" . "673ab7")
    ("deepPurple[600]" . "5e35b1")
    ("deepPurple[700]" . "512da8")
    ("deepPurple[800]" . "4527a0")
    ("deepPurple[900]" . "311b92")
    ("indigo" . "3f51b5")
    ("indigo[50]" . "e8eaf6")
    ("indigo[100]" . "c5cae9")
    ("indigo[200]" . "9fa8da")
    ("indigo[300]" . "7986cb")
    ("indigo[400]" . "5c6bc0")
    ("indigo[500]" . "3f51b5")
    ("indigo[600]" . "3949ab")
    ("indigo[700]" . "303f9f")
    ("indigo[800]" . "283593")
    ("indigo[900]" . "1a237e")
    ("blue" . "2196f3")
    ("blue[50]" . "e3f2fd")
    ("blue[100]" . "bbdefb")
    ("blue[200]" . "90caf9")
    ("blue[300]" . "64b5f6")
    ("blue[400]" . "42a5f5")
    ("blue[500]" . "2196f3")
    ("blue[600]" . "1e88e5")
    ("blue[700]" . "1976d2")
    ("blue[800]" . "1565c0")
    ("blue[900]" . "0d47a1")
    ("lightBlue" . "03a9f4")
    ("lightBlue[50]" . "e1f5fe")
    ("lightBlue[100]" . "b3e5fc")
    ("lightBlue[200]" . "81d4fa")
    ("lightBlue[300]" . "4fc3f7")
    ("lightBlue[400]" . "29b6f6")
    ("lightBlue[500]" . "03a9f4")
    ("lightBlue[600]" . "039be5")
    ("lightBlue[700]" . "0288d1")
    ("lightBlue[800]" . "0277bd")
    ("lightBlue[900]" . "01579b")
    ("cyan" . "00bcd4")
    ("cyan[50]" . "e0f7fa")
    ("cyan[100]" . "b2ebf2")
    ("cyan[200]" . "80deea")
    ("cyan[300]" . "4dd0e1")
    ("cyan[400]" . "26c6da")
    ("cyan[500]" . "00bcd4")
    ("cyan[600]" . "00acc1")
    ("cyan[700]" . "0097a7")
    ("cyan[800]" . "00838f")
    ("cyan[900]" . "006064")
    ("teal" . "009688")
    ("teal[50]" . "e0f2f1")
    ("teal[100]" . "b2dfdb")
    ("teal[200]" . "80cbc4")
    ("teal[300]" . "4db6ac")
    ("teal[400]" . "26a69a")
    ("teal[500]" . "009688")
    ("teal[600]" . "00897b")
    ("teal[700]" . "00796b")
    ("teal[800]" . "00695c")
    ("teal[900]" . "004d40")
    ("green" . "4caf50")
    ("green[50]" . "e8f5e9")
    ("green[100]" . "c8e6c9")
    ("green[200]" . "a5d6a7")
    ("green[300]" . "81c784")
    ("green[400]" . "66bb6a")
    ("green[500]" . "4caf50")
    ("green[600]" . "43a047")
    ("green[700]" . "388e3c")
    ("green[800]" . "2e7d32")
    ("green[900]" . "1b5e20")
    ("lightGreen" . "8bc34a")
    ("lightGreen[50]" . "f1f8e9")
    ("lightGreen[100]" . "dcedc8")
    ("lightGreen[200]" . "c5e1a5")
    ("lightGreen[300]" . "aed581")
    ("lightGreen[400]" . "9ccc65")
    ("lightGreen[500]" . "8bc34a")
    ("lightGreen[600]" . "7cb342")
    ("lightGreen[700]" . "689f38")
    ("lightGreen[800]" . "558b2f")
    ("lightGreen[900]" . "33691e")
    ("lime" . "cddc39")
    ("lime[50]" . "f9fbe7")
    ("lime[100]" . "f0f4c3")
    ("lime[200]" . "e6ee9c")
    ("lime[300]" . "dce775")
    ("lime[400]" . "d4e157")
    ("lime[500]" . "cddc39")
    ("lime[600]" . "c0ca33")
    ("lime[700]" . "afb42b")
    ("lime[800]" . "9e9d24")
    ("lime[900]" . "827717")
    ("yellow" . "ffeb3b")
    ("yellow[50]" . "fffde7")
    ("yellow[100]" . "fff9c4")
    ("yellow[200]" . "fff59d")
    ("yellow[300]" . "fff176")
    ("yellow[400]" . "ffee58")
    ("yellow[500]" . "ffeb3b")
    ("yellow[600]" . "fdd835")
    ("yellow[700]" . "fbc02d")
    ("yellow[800]" . "f9a825")
    ("yellow[900]" . "f57f17")
    ("amber" . "ffc107")
    ("amber[50]" . "fff8e1")
    ("amber[100]" . "ffecb3")
    ("amber[200]" . "ffe082")
    ("amber[300]" . "ffd54f")
    ("amber[400]" . "ffca28")
    ("amber[500]" . "ffc107")
    ("amber[600]" . "ffb300")
    ("amber[700]" . "ffa000")
    ("amber[800]" . "ff8f00")
    ("amber[900]" . "ff6f00")
    ("orange" . "ff9800")
    ("orange[50]" . "fff3e0")
    ("orange[100]" . "ffe0b2")
    ("orange[200]" . "ffcc80")
    ("orange[300]" . "ffb74d")
    ("orange[400]" . "ffa726")
    ("orange[500]" . "ff9800")
    ("orange[600]" . "fb8c00")
    ("orange[700]" . "f57c00")
    ("orange[800]" . "ef6c00")
    ("orange[900]" . "e65100")
    ("deepOrange" . "ff5722")
    ("deepOrange[50]" . "fbe9e7")
    ("deepOrange[100]" . "ffccbc")
    ("deepOrange[200]" . "ffab91")
    ("deepOrange[300]" . "ff8a65")
    ("deepOrange[400]" . "ff7043")
    ("deepOrange[500]" . "ff5722")
    ("deepOrange[600]" . "f4511e")
    ("deepOrange[700]" . "e64a19")
    ("deepOrange[800]" . "d84315")
    ("deepOrange[900]" . "bf360c")
    ("brown" . "795548")
    ("brown[50]" . "efebe9")
    ("brown[100]" . "d7ccc8")
    ("brown[200]" . "bcaaa4")
    ("brown[300]" . "a1887f")
    ("brown[400]" . "8d6e63")
    ("brown[500]" . "795548")
    ("brown[600]" . "6d4c41")
    ("brown[700]" . "5d4037")
    ("brown[800]" . "4e342e")
    ("brown[900]" . "3e2723")
    ("blueGrey" . "607d8b")
    ("blueGrey[50]" . "eceff1")
    ("blueGrey[100]" . "cfd8dc")
    ("blueGrey[200]" . "b0bec5")
    ("blueGrey[300]" . "90a4ae")
    ("blueGrey[400]" . "78909c")
    ("blueGrey[500]" . "607d8b")
    ("blueGrey[600]" . "546e7a")
    ("blueGrey[700]" . "455a64")
    ("blueGrey[800]" . "37474f")
    ("blueGrey[900]" . "263238")
    ("redAccent" . "ff5252")
    ("redAccent[100]" . "ff8a80")
    ("redAccent[200]" . "ff5252")
    ("redAccent[400]" . "ff1744")
    ("redAccent[700]" . "d50000")
    ("pinkAccent" . "ff4081")
    ("pinkAccent[100]" . "ff80ab")
    ("pinkAccent[200]" . "ff4081")
    ("pinkAccent[400]" . "f50057")
    ("pinkAccent[700]" . "c51162")
    ("purpleAccent" . "e040fb")
    ("purpleAccent[100]" . "ea80fc")
    ("purpleAccent[200]" . "e040fb")
    ("purpleAccent[400]" . "d500f9")
    ("purpleAccent[700]" . "aa00ff")
    ("deepPurpleAccent" . "7c4dff")
    ("deepPurpleAccent[100]" . "b388ff")
    ("deepPurpleAccent[200]" . "7c4dff")
    ("deepPurpleAccent[400]" . "651fff")
    ("deepPurpleAccent[700]" . "6200ea")
    ("indigoAccent" . "536dfe")
    ("indigoAccent[100]" . "8c9eff")
    ("indigoAccent[200]" . "536dfe")
    ("indigoAccent[400]" . "3d5afe")
    ("indigoAccent[700]" . "304ffe")
    ("blueAccent" . "448aff")
    ("blueAccent[100]" . "82b1ff")
    ("blueAccent[200]" . "448aff")
    ("blueAccent[400]" . "2979ff")
    ("blueAccent[700]" . "2962ff")
    ("lightBlueAccent" . "40c4ff")
    ("lightBlueAccent[100]" . "80d8ff")
    ("lightBlueAccent[200]" . "40c4ff")
    ("lightBlueAccent[400]" . "00b0ff")
    ("lightBlueAccent[700]" . "0091ea")
    ("cyanAccent" . "18ffff")
    ("cyanAccent[100]" . "84ffff")
    ("cyanAccent[200]" . "18ffff")
    ("cyanAccent[400]" . "00e5ff")
    ("cyanAccent[700]" . "00b8d4")
    ("tealAccent" . "64ffda")
    ("tealAccent[100]" . "a7ffeb")
    ("tealAccent[200]" . "64ffda")
    ("tealAccent[400]" . "1de9b6")
    ("tealAccent[700]" . "00bfa5")
    ("greenAccent" . "69f0ae")
    ("greenAccent[100]" . "b9f6ca")
    ("greenAccent[200]" . "69f0ae")
    ("greenAccent[400]" . "00e676")
    ("greenAccent[700]" . "00c853")
    ("lightGreenAccent" . "b2ff59")
    ("lightGreenAccent[100]" . "ccff90")
    ("lightGreenAccent[200]" . "b2ff59")
    ("lightGreenAccent[400]" . "76ff03")
    ("lightGreenAccent[700]" . "64dd17")
    ("limeAccent" . "eeff41")
    ("limeAccent[100]" . "f4ff81")
    ("limeAccent[200]" . "eeff41")
    ("limeAccent[400]" . "c6ff00")
    ("limeAccent[700]" . "aeea00")
    ("yellowAccent" . "ffff00")
    ("yellowAccent[100]" . "ffff8d")
    ("yellowAccent[200]" . "ffff00")
    ("yellowAccent[400]" . "ffea00")
    ("yellowAccent[700]" . "ffd600")
    ("amberAccent" . "ffd740")
    ("amberAccent[100]" . "ffe57f")
    ("amberAccent[200]" . "ffd740")
    ("amberAccent[400]" . "ffc400")
    ("amberAccent[700]" . "ffab00")
    ("orangeAccent" . "ffab40")
    ("orangeAccent[100]" . "ffd180")
    ("orangeAccent[200]" . "ffab40")
    ("orangeAccent[400]" . "ff9100")
    ("orangeAccent[700]" . "ff6d00")
    ("deepOrangeAccent" . "ff6e40")
    ("deepOrangeAccent[100]" . "ff9e80")
    ("deepOrangeAccent[200]" . "ff6e40")
    ("deepOrangeAccent[400]" . "ff3d00")
    ("deepOrangeAccent[700]" . "dd2c00")))

(provide 'lsp-dart-flutter-colors)
;;; lsp-dart-flutter-colors.el ends here
