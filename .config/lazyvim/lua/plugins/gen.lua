return {
	{
		-- Minimal configuration
		"David-Kunz/gen.nvim",
		opts = {
			-- Model configuration
			-- Mistral: gen.nvim Default model for general use
			-- 7B: Good accuracy
			-- model = "mistral",
			-- Llama: General use
			-- 3.2 3B: Balance of speed, hardware requirements
			-- model = "llama3.2",
			-- codeqwen: Code generation, code use cases
			-- 1.5 7B
			-- model = "codeqwen"
			-- phi3: Lightweight general use
			-- 3Bi (mini) and 14B for hardware constrained environments
			model = "phi3:mini",
		},
	},
}
