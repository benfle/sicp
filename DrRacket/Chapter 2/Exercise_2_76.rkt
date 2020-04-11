#lang scheme

;
; For "explicit dispatch" style:
;  New type: We need to modify every operations to add the new type in the "cond" expression.
;  New operation: We need to write the new operation that dispatch on all existing types.
;
; For "data-directed" style:
;  New type: We need to implement the existing generic operations for the new type and declare them in the dispatch table.
;  New operation: We need to implement the new generic operation for each existing type and declare them in the dispatching table.
;
; For "message-passing" style:
;  New type: We need to write the new type and its operations.
;  New operation: We need to add the operation to each type that supports it
;
; Appropriate style for a system in which new types must often be added?
;  The most appropriate style is the "message-passing" style because we only have to write the new type and the operations it supports.
;  The worst choice would be an "explicit dispatch" style where you have to modify every operations that support the new type.
;  The data-directed style is also acceptable.
;
; Appropriate style for a system in which new operations must often be added?
;  The most appropriate style is the "data-directed" style but the "explicit dispatch" is also acceptable since we only have the new operation
;  to implement. In the "message-passing" style we would have to modify all existing types that support this new operation.