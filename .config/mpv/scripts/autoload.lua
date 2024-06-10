-- From https://raw.githubusercontent.com/mpv-player/mpv/master/TOOLS/lua/autoload.lua
-- file information: https://github.com/mpv-player/mpv/blob/master/TOOLS/lua/autoload.lua
-- Using license GPL: https://github.com/mpv-player/mpv/blob/master/LICENSE.GPL
-- Updated 2024-06-10
-- This script automatically loads playlist entries before and after the
-- currently played file. It does so by scanning the directory a file is
-- located in when starting playback. It sorts the directory entries
-- alphabetically, and adds entries before and after the current file to
-- the internal playlist. (It stops if it would add an already existing
-- playlist entry at the same position - this makes it "stable".)
-- Add at most 5000 * 2 files when starting a file (before + after).

--[[
To configure this script use file autoload.conf in directory script-opts (the "script-opts"
directory must be in the mpv configuration directory, typically ~/.config/mpv/).

Option `ignore_patterns` is a comma-separated list of patterns (see lua.org/pil/20.2.html).
Additionally to the standard lua patterns, you can also escape commas with `%`,
for example, the option `bak%,x%,,another` will be resolved as patterns `bak,x,` and `another`.
But it does not mean you need to escape all lua patterns twice,
so the option `bak%%,%.mp4,` will be resolved as two patterns `bak%%` and `%.mp4`.

Example configuration would be:

disabled=no
images=no
videos=yes
audio=yes
additional_image_exts=list,of,ext
additional_video_exts=list,of,ext
additional_audio_exts=list,of,ext
ignore_hidden=yes
same_type=yes
directory_mode=recursive
ignore_patterns=^~,^bak-,%.bak$

--]]

local MAX_ENTRIES = 5000
local MAX_DIR_STACK = 20

local msg = require 'mp.msg'
local options = require 'mp.options'
local utils = require 'mp.utils'

local o = {
    disabled = false,
    images = true,
    videos = true,
    audio = true,
    additional_image_exts = "",
    additional_video_exts = "",
    additional_audio_exts = "",
    ignore_hidden = true,
    same_type = false,
    directory_mode = "auto",
    ignore_patterns = ""
}

local function Set(t)
    local set = {}
    for _, v in pairs(t) do set[v] = true end
    return set
end

local EXTENSIONS_VIDEO_DEFAULT = Set {
    '3g2', '3gp', 'avi', 'flv', 'm2ts', 'm4v', 'mj2', 'mkv', 'mov',
    'mp4', 'mpeg', 'mpg', 'ogv', 'rmvb', 'webm', 'wmv', 'y4m'
}

local EXTENSIONS_AUDIO_DEFAULT = Set {
    'aiff', 'ape', 'au', 'flac', 'm4a', 'mka', 'mp3', 'oga', 'ogg',
    'ogm', 'opus', 'wav', 'wma'
}

local EXTENSIONS_IMAGES_DEFAULT = Set {
    'avif', 'bmp', 'gif', 'j2k', 'jp2', 'jpeg', 'jpg', 'jxl', 'png',
    'svg', 'tga', 'tif', 'tiff', 'webp'
}

local EXTENSIONS, EXTENSIONS_VIDEO, EXTENSIONS_AUDIO, EXTENSIONS_IMAGES

local function SetUnion(a, b)
    for k in pairs(b) do a[k] = true end
    return a
end

-- Returns first and last positions in string or past-to-end indices
local function FindOrPastTheEnd(string, pattern, start_at)
    local pos1, pos2 = string:find(pattern, start_at)