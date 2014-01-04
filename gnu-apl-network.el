;;; -*- lexical-binding: t -*-

(defvar *gnu-apl-end-tag* "APL_NATIVE_END_TAG")

(defun gnu-apl--connect (port)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (when (and (boundp 'gnu-apl--connection)
               (process-live-p gnu-apl--connection))
      (error "Connection is already established"))
    (let ((proc (open-network-stream "*gnu-apl-connection*" nil "localhost" 7293
                                       :type 'plain
                                       :return-list nil
                                       :end-of-command "\n")))
      (set-process-filter proc 'gnu-apl--filter-network)
      (set (make-local-variable 'gnu-apl--connection) proc)
      (set (make-local-variable 'gnu-apl--current-incoming) "")
      (set (make-local-variable 'gnu-apl--results) nil))))

(defun gnu-apl--filter-network (proc output)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (setq gnu-apl--current-incoming (concat gnu-apl--current-incoming output))
    (llog "Processing input: %S" output)
    (loop with start = 0
          for pos = (cl-position ?\n gnu-apl--current-incoming :start start)
          do (llog "pos=%S" pos)
          while pos
          do (let ((s (subseq gnu-apl--current-incoming start pos)))
               (llog "s=%S" s)
               (setq start (1+ pos))
               (llog "new start: %S" start)
               (setq gnu-apl--results (nconc gnu-apl--results (list s)))
               (llog "new restults: %S" gnu-apl--results))
          finally (when (plusp start)
                    (setq gnu-apl--current-incoming (subseq gnu-apl--current-incoming start))))))

(defun gnu-apl--send-network-command (command)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (llog "OUT:%S (+NL)" command)
    (process-send-string gnu-apl--connection (concat command "\n"))))

(defun gnu-apl--send-block (lines)
  (dolist (line lines)
    (gnu-apl--send-network-command line))
  (gnu-apl--send-network-command *gnu-apl-end-tag*)
  (llog "OUT:BLOCK SENT"))

(defun gnu-apl--read-network-reply ()
  (with-current-buffer (gnu-apl--get-interactive-session)
    (loop while (null gnu-apl--results)
          do (accept-process-output gnu-apl--connection 3))
    (let ((value (pop gnu-apl--results)))
      (llog "IN:%S" value)
      value)))

(defun gnu-apl--read-network-reply-block ()
  (prog1
      (loop for line = (gnu-apl--read-network-reply)
            while (not (string= line *gnu-apl-end-tag*))
            collect line)
    (llog "IN:BLOCK READY")))
