-- https://github.com/mason-org/mason.nvim/discussions/2116
-- Load mason in LazyVim, get installed package list from Mason's API
-- Update mason registeries, then reinstall updated packages
-- whose installed version is different from latest
local registry = require("mason-registry")
local pkg_to_be_updated = {}

for _, pkg in ipairs(registry.get_installed_packages()) do
  if pkg:get_installed_version() ~= pkg:get_latest_version() then
    table.insert(pkg_to_be_updated, pkg.name)
  end
end
if #pkg_to_be_updated > 0 then
  vim.cmd("MasonInstall " .. table.concat(pkg_to_be_updated, " "))
end
