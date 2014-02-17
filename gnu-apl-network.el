;;; -*- lexical-binding: t -*-

(defvar *gnu-apl-end-tag* "APL_NATIVE_END_TAG")
(defvar *gnu-apl-notification-start* "APL_NATIVE_NOTIFICATION_START")
(defvar *gnu-apl-notification-end* "APL_NATIVE_NOTIFICATION_END")

(defun gnu-apl--connect-to-remote (connect-mode addr)
  (cond ((string= connect-mode "tcp")
         (open-network-stream "*gnu-apl-connection*" nil "localhost" (string-to-number addr)
                              :type 'plain
                              :return-list nil
                              :end-of-command "\n"))
        ((string= connect-mode "unix")
         (make-network-process :name "gnu-apl-native"
                               :buffer "*gnu-apl-connection*"
                               :family 'local
                               :type nil
                               :service addr
                               :coding 'utf-8))
        (t
         (error "Unexpected connect mode: %s" connect-mode))))

(defun gnu-apl--connect (connect-mode addr)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (when (and (boundp 'gnu-apl--connection)
               (process-live-p gnu-apl--connection))
      (error "Connection is already established"))
    (condition-case err
        (let ((proc (gnu-apl--connect-to-remote connect-mode addr)))
          (set-process-coding-system proc 'utf-8 'utf-8)
          (set-process-filter proc 'gnu-apl--filter-network)
          (set (make-local-variable 'gnu-apl--connection) proc)
          (set (make-local-variable 'gnu-apl--current-incoming) "")
          (set (make-local-variable 'gnu-apl--results) nil))
      ;; TODO: Error handling is pretty poor right now
      ('file-error (error "err:%S type:%S" err (type-of err))))))

(defun gnu-apl--process-notification (lines)
  (llog "Got notification: %S" lines))

(defun gnu-apl--find-notification ()
  (let ((start (cl-find *gnu-apl-notification-start* gnu-apl--current-incoming :test #'string=)))
    (when start
      (let ((end (cl-find *gnu-apl-notification-end* gnu-apl--current-incoming
                          :test #'string=
                          :start (1+ start))))
        (when end
          (list start end))))))

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
                    (setq gnu-apl--current-incoming (subseq gnu-apl--current-incoming start))))
    (loop for pos = (gnu-apl--find-notification)
          while pos
          do (progn
               (gnu-apl--process-notification (subseq gnu-apl--current-incoming
                                                      (1+ (car pos)) (1- (cadr pos))))
               (setq gnu-apl--current-incoming (append (subseq gnu-apl--current-incoming 0 (car pos))
                                                       (subseq gnu-apl--current-incoming (cadr pos))))))))

(defun gnu-apl--send-network-command (command)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (process-send-string gnu-apl--connection (concat command "\n"))))

(defun gnu-apl--send-block (lines)
  (dolist (line lines)
    (gnu-apl--send-network-command line))
  (gnu-apl--send-network-command *gnu-apl-end-tag*))

(defun gnu-apl--read-network-reply ()
  (with-current-buffer (gnu-apl--get-interactive-session)
    (loop while (or (null gnu-apl--results))
          do (accept-process-output gnu-apl--connection 3))
    (let ((value (pop gnu-apl--results)))
      value)))

(defun gnu-apl--read-network-reply-block ()
  (loop for line = (gnu-apl--read-network-reply)
        while (not (string= line *gnu-apl-end-tag*))
        collect line))
