;;; -*- lexical-binding: t -*-

(defun gnu-apl--connect (port)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (when (and (boundp 'gnu-apl--connection)
               (process-live-p gnu-apl--connection))
      (error "Connection is already established"))
    (let ((proc (open-network-stream "*gnu-apl-connection*" nil "localhost" 7293
                                       :type 'plain
                                       :return-list nil
                                       :end-of-command "\n")))
      (set-process-filter proc #'gnu-apl--filter-network)
      (set (make-local-variable 'gnu-apl--connection) proc)
      (set (make-local-variable 'gnu-apl--current-incoming) nil)
      (set (make-local-variable 'gnu-apl--ignore-num) 0)
      (set (make-local-variable 'gnu-apl--results) nil))))

(defun gnu-apl--filter-network (proc output)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (let* ((parts (split-string output "\n")))
      (dolist (v parts)
        (llog "Got part: %S" v)
        (if (string= v "END")
            (progn
              (setq gnu-apl--results (append gnu-apl--results (list gnu-apl--current-incoming)))
              (setq gnu-apl--current-incoming nil))
          (progn
            (setq gnu-apl--current-incoming (append gnu-apl--current-incoming (list v))))))
      (loop while (and (plusp gnu-apl--ignore-num)
                       gnu-apl--results)
            do (progn 
                 (pop gnu-apl--results)
                 (decf gnu-apl--ignore-num))))))

(defun gnu-apl--send-network-command (command)
  (with-current-buffer (gnu-apl--get-interactive-session)
    (process-send-string gnu-apl--connection (concat command "\n"))
    (loop while (null gnu-apl--results)
          do (accept-process-output gnu-apl--connection 3))
    (unless gnu-apl--results
      (incf gnu-apl--ignore-num)
      (error "Backend connection did not reply in time"))
    (pop gnu-apl--results)))
