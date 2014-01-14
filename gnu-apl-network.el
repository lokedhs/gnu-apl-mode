;;; -*- lexical-binding: t -*-

(defvar *gnu-apl-end-tag* "APL_NATIVE_END_TAG")

(defun gnu-apl--connect-to-remote (connect-mode addr)
  (cond ((string= connect-mode "tcp")
         (open-network-stream "*gnu-apl-connection*" nil "localhost" (string-to-number addr)
                              :type 'plain
                              :return-list nil
                              :end-of-command "\n"))
        (t
         (error "Unexpected connect mode: %s" connect-mode))))

(defun gnu-apl--connect (connect-mode addr)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (when (and (boundp 'gnu-apl--connection)
               (process-live-p gnu-apl--connection))
      (error "Connection is already established"))
    (condition-case err
        (let ((proc (gnu-apl--connect-to-remote connect-mode addr)))
          (set-process-filter proc 'gnu-apl--filter-network)
          (set (make-local-variable 'gnu-apl--connection) proc)
          (set (make-local-variable 'gnu-apl--current-incoming) "")
          (set (make-local-variable 'gnu-apl--results) nil))
      ;; TODO: Error handling is pretty poor right now
        ('file-error (error "err:%S type:%S" err (type-of err))))))

(defun gnu-apl--filter-network (proc output)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (setq gnu-apl--current-incoming (concat gnu-apl--current-incoming output))
    (loop with start = 0
          for pos = (cl-position ?\n gnu-apl--current-incoming :start start)
          while pos
          do (let ((s (subseq gnu-apl--current-incoming start pos)))
               (setq start (1+ pos))
               (setq gnu-apl--results (nconc gnu-apl--results (list s))))
          finally (when (plusp start)
                    (setq gnu-apl--current-incoming (subseq gnu-apl--current-incoming start))))))

(defun gnu-apl--send-network-command (command)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (process-send-string gnu-apl--connection (concat command "\n"))))

(defun gnu-apl--send-block (lines)
  (dolist (line lines)
    (gnu-apl--send-network-command line))
  (gnu-apl--send-network-command *gnu-apl-end-tag*))

(defun gnu-apl--read-network-reply ()
  (with-current-buffer (gnu-apl--get-interactive-session)
    (loop while (null gnu-apl--results)
          do (accept-process-output gnu-apl--connection 3))
    (let ((value (pop gnu-apl--results)))
      value)))

(defun gnu-apl--read-network-reply-block ()
  (loop for line = (gnu-apl--read-network-reply)
        while (not (string= line *gnu-apl-end-tag*))
        collect line))
