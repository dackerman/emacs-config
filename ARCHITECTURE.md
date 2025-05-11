# Inkling Architecture

## Overview

Inkling is an intelligent code assistance tool for Emacs that provides context-aware suggestions for code improvements. Unlike traditional autocomplete tools, Inkling aims to understand what the user is trying to do by monitoring their edits, tracking LSP diagnostics, and analyzing patterns in the code.

## Core Philosophy

1. **Context over Constant** - Inkling should make suggestions based on understanding the user's intent, not just based on the current state of the code. 

2. **Latent Intent Recognition** - By monitoring the sequence of edits a user makes, Inkling can recognize patterns and help complete similar tasks elsewhere in the codebase.

3. **Error-Driven Assistance** - When compiler/linter errors appear, Inkling should understand them and offer fixes that align with the user's coding style and intentions.

4. **Unobtrusive by Default** - Suggestions should appear when they're relevant, not constantly. The system should minimize false positives.

5. **Edit History as Context** - The history of recent edits provides crucial context for what the user is trying to accomplish.

## System Components

### 1. Edit Tracking Subsystem

The edit tracking system records all edits made to a buffer while it's open:

- Each edit is classified by type (add, delete, modify, refactor)
- Edits include surrounding context, position, before/after text, and timestamp
- Edit history is buffer-local and cleared when the buffer is closed
- Patterns in the edits are analyzed to determine user intent

```
Edit Record = {
  type: one of [add, delete, modify, refactor, error-fix],
  position: buffer position,
  before: text before change,
  after: text after change,
  timestamp: when edit occurred,
  context: surrounding code,
  lsp-data: any LSP diagnostics at the time
}
```

### 2. LSP Integration

Inkling monitors LSP errors and warnings in real-time:

- New errors after an edit are analyzed to understand what might need fixing
- The system correlates errors with recent edits to understand cause-effect
- When suggesting fixes, Inkling references similar fixes the user has made elsewhere

### 3. Suggestion Engine

The suggestion engine analyzes edit history and LSP data:

- **Pattern Matching**: Identifies repeated edit patterns (e.g., refactoring a variable name)
- **Error Resolution**: Suggests fixes for LSP errors based on context and past fixes
- **Continuation**: Recognizes multi-step operations and suggests completing them
- **Similar Code Detection**: Finds code similar to what was just edited and suggests similar changes

### 4. User Interface

Inkling presents suggestions through a non-intrusive UI:

- Highlighted regions show where changes would be applied
- Preview functionality shows the outcome before accepting
- Simple keybindings for accept/reject/preview
- Focuses on changes the user is likely to want, not constant suggestions

## Workflow

1. User makes edits to the code
2. Inkling records these edits and any resulting LSP errors
3. The system analyzes patterns in the edits and errors
4. When a clear pattern emerges, Inkling generates a suggestion
5. The affected region is highlighted
6. User can preview, accept, or dismiss the suggestion

## Technical Implementation

### Edit Recording

Edit recording happens through Emacs' `after-change-functions` hook:

```elisp
(defun inkling--record-edit (beg end length)
  "Record an edit that occurred between BEG and END with prior LENGTH."
  ;; Implementation details...
)

;; Hook it up
(add-hook 'after-change-functions #'inkling--record-edit nil t)
```

### LSP Monitoring

LSP diagnostics are monitored through lsp-mode hooks:

```elisp
(add-hook 'lsp-diagnostics-updated-hook #'inkling--analyze-diagnostics nil t)
```

### Pattern Analysis

Edit patterns are analyzed using a combination of:

- Textual similarity (for similar code edits)
- Positional analysis (edits in similar code constructs)
- Error-edit correlation (which edits fix which errors)
- Common refactoring patterns (e.g., rename, extract function)

### Suggestion Generation

Suggestions are generated reactively:

1. After a sequence of similar edits (to suggest continuing the pattern)
2. After an edit that introduces an error (to suggest a fix)
3. After modifying code that has similar structures elsewhere (to suggest similar changes)

## Future Directions

1. **ML-Enhanced Patterns** - Using machine learning to enhance pattern recognition
2. **Project-Wide Context** - Extending context beyond the current buffer
3. **Customization Profiles** - Learning individual user patterns and preferences
4. **Multi-Step Suggestions** - Suggesting sequences of related changes
5. **Integration with Version Control** - Using commit history for additional context

## Design Decisions

### Why Buffer-Local History?

Buffer-local history provides:
- Clean separation between different files
- Automatic cleanup when files are closed
- Focused context on the current task
- Simpler implementation and state management

### Why Classification of Edit Types?

Classifying edits helps:
- Recognize higher-level patterns (e.g., refactoring vs. bug fixing)
- Filter relevant edits for suggestion generation
- Prioritize suggestions based on what the user is currently doing

### Why Highlight Instead of Inline/Popup?

Highlighting affected regions:
- Works in any display context (including small areas like minibuffer)
- Doesn't disrupt code layout or reading flow
- Clearly shows the extent of the proposed change
- Provides a clear target for preview/accept actions