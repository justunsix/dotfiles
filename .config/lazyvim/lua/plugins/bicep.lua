vim.filetype.add({
  extension = {
    bicep = "bicep",
  },
  filename = {
    ["*.bicep"] = "bicep",
  },
})
return {
  "carlsmedstad/vim-bicep",
  ft = "bicep",
}
