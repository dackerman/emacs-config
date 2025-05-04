# Emacs Config Project Information

## Environment Details
- ~/.emacs.d is symlinked to /home/david/code/emacs-config, so they are the same directory
- GNU Emacs 30.1
- Platform: Linux 6.12.25 (NixOS)

## Project Structure
- Main configuration loaded from `installations/home-nixos-desktop.el`
- Features defined in `lisp/features.el` as modular components
- Cursor-assist package in `lisp/cursor-assist.el` for AI code assistance

## Package Management
- Uses straight.el for package management
- Configured in the bootstrap-straight-el function in features.el

## Testing
- Run `make test` to run all tests
- Run `make test-cursor-assist` to test just the cursor-assist package
- Run `make lint` to check Emacs Lisp syntax

## Key Components
- `cursor-assist.el`: Provides Cursor-like LLM code assistance
- `features.el`: Contains modular features that can be enabled selectively
- `installations/home-nixos-desktop.el`: Main configuration for David's NixOS desktop

## LLM Integration
- Uses gptel for LLM integration
- Configured to work with Claude, OpenAI and Gemini
- cursor-assist uses Claude for code suggestions

## Important Commands
- `M-x cursor-assist-mode` or `C-c C-a`: Toggle cursor-assist mode
- `C-c g`: Launch gptel
- `C-c s`: Send gptel message
- `C-c p`: Projectile command prefix
- `C-c t`: Show treemacs with current project
- `C-c m s`: Launch magit status

## Other Notes
- LSP mode configured for multiple languages
- Company mode used for completion
- Programming language support includes:
  - Clojure/ClojureScript
  - TypeScript/JavaScript
  - Python
  - Go
  - Emacs Lisp
  - and more