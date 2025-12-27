# AGENTS.md

This file contains guidelines for agentic coding agents working on the head-context-sticky Emacs package.

## Project Overview

This is an Emacs Lisp package that provides sticky context headers for Emacs buffers, similar to VS Code's sticky scroll feature. It displays contextually relevant information (function definitions, class definitions, etc.) in a floating frame at the top of windows.

## Build/Lint/Test Commands

This is a single-file Emacs Lisp package with no traditional build system. Use these commands for development:

### Testing
```bash
# Load the package for manual testing
emacs -Q -l head-context-sticky.el

# Check syntax
emacs --batch --eval "(progn (load-file \"head-context-sticky.el\") (message \"Syntax check passed\"))"

# Byte compile (shows warnings/errors)
emacs --batch --eval "(byte-compile-file \"head-context-sticky.el\")"
```

### Linting
```bash
# Check with package-lint (if available)
emacs --batch --eval "(progn (require 'package-lint) (package-lint-buffer \"head-context-sticky.el\"))"
```

## Code Style Guidelines

### File Structure
- Use section dividers: `;; -----------------------------------------------------------------------------`
- Number sections clearly: `;; 1. Configuration`, `;; 2. Core Logic`, etc.
- Use `;;` for comments, `;;;` for section headers, `;;;;` for subsections

### Naming Conventions
- **Variables**: Use kebab-case for user-facing variables (`head-context-sticky-max-lines`)
- **Internal functions**: Use double dashes (`head-context-sticky--get-context-data`)
- **Commands**: Use plain names without dashes for interactive functions
- **Faces**: End with `-face` suffix (`head-context-sticky-number-face`)

### Function Organization
- Group related functions together
- Use `defcustom` for user configurations at the top
- Use `defface` for face definitions
- Private functions should use `--` prefix
- Helper functions should be defined near their main usage

### Emacs Lisp Specific Patterns

#### Error Handling
- Use `condition-case nil ... (error nil)` for graceful degradation
- Wrap potentially failing operations in `ignore-errors` when appropriate

#### Buffer/Window Management
- Always use `with-selected-window` or `with-current-buffer` for temporary context switches
- Use `save-excursion` and `save-restriction` for preserving point/limits
- Check object liveness: `(window-live-p win)`, `(frame-live-p frame)`

#### Performance Considerations
- Use hash tables for lookups: `(make-hash-table :test 'eq :weakness 'key)`
- Cache expensive computations with global variables
- Use idle timers for deferred updates: `run-with-idle-timer`
- Limit operations with counters to prevent infinite loops

#### Tree-sitter Integration
- Check availability: `(and (featurep 'treesit) (treesit-parser-list))`
- Handle missing parsers gracefully
- Use `treesit-node-at`, `treesit-node-parent`, `treesit-node-type`

### Code Quality Standards

#### Required Headers
```elisp
;;; head-context-sticky.el --- Description -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "29.4"))

;;; Commentary:

;;; Code:
```

#### Dependencies
- Only require what's used: `treesit`, `seq`, `subr-x`, `color`
- Check for optional features before using
- Minimum Emacs version: 29.4 (for tree-sitter support)

#### Configuration Pattern
- Use `defgroup` for related settings
- Provide sensible defaults
- Use appropriate types: `:type 'integer`, `:type 'boolean`, `:type '(repeat symbol)`

#### Minor Mode Pattern
```elisp
;;;###autoload
(define-minor-mode head-context-sticky-mode
  "Description."
  :global nil
  (if head-context-sticky-mode
      ;; Enable code
    ;; Disable code
    ))

;;;###autoload
(define-globalized-minor-mode global-head-context-sticky-mode
  head-context-sticky-mode
  (lambda () (unless (minibufferp) (head-context-sticky-mode +1))))
```

### Testing Guidelines

Since there are no automated tests:
- Test manually with different major modes
- Verify tree-sitter integration with supported languages
- Test edge cases: empty buffers, read-only buffers, special modes
- Check memory leaks by monitoring garbage collection
- Test with different Emacs themes and font sizes

### Common Patterns to Follow

#### Rendering Optimization
- Cache background colors: store in global variable with cache invalidation
- Use fingerprinting to avoid unnecessary re-renders
- Implement content change detection before expensive operations

#### Frame Management
- Use weak hash tables for window->frame mappings
- Always clean up frames and buffers on window deletion
- Store parent window reference using `frame-parameter`

#### Event Handling
- Use idle timers to avoid jank during typing
- Implement proper event forwarding for mouse interactions
- Handle edge cases like frame visibility and focus

## Notes for AI Agents

- This package uses advanced Emacs features: tree-sitter, child frames, weak hash tables
- Performance is critical - avoid blocking operations during user interaction
- The code contains Chinese comments in some sections - preserve them when making changes
- When adding new node types to `head-context-sticky-node-rules`, follow the existing structure
- Always test with both tree-sitter enabled and disabled (fallback modes)
- The package should gracefully degrade when tree-sitter is unavailable