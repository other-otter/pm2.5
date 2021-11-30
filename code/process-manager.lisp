(in-package :process-stream)

(defun start-process-manager ()
    (defvar *process-map* (make-hash-table))
    (defvar *uid-map*     (make-hash-table :test #'equal)))

(defun add-program (the-list)
    (let ((file-path (uiop:file-exists-p (getf the-list :path)))))
        (if file-path
            (let* ( (the-process (sb-ext:run-program file-path (getf the-list :args) :pty t :external-format :utf-8))
                    (process-pid (sb-ext:process-pid the-process))
                    (process-pty (sb-ext:process-pty the-process))
                    (restart-thread (sb-thread:make-thread (lambda () (restart-program (sb-ext:process-wait the-process))))))
                    (setf   (gethash program-id *uid-map*) 
                            (list :uid (getf the-list :uid) :path (getf the-list :path) :args (getf the-list :args) :retry (getf the-list :retry) :pid process-pid :main the-process :stay t :restart restart-thread :pty process-pty :note nil) 
                            (gethash the-process *process-map*)
                            program-id)
            nil)))

(defun restart-program (the-process)
    (let* ( (program-name   (gethash the-process *process-map*))
            (program-info   (gethash program-name *uid-map*))
            (program-stay   (getf program-info :stay))
            (program-retry  (getf program-info :retry)))
        (if (and program-stay program-retry)
            (let* ( (program-path (getf program-info :path))
                    (program-args (getf program-info :args))
                    (restart-process (sb-ext:run-program program-path program-args :pty t :external-format :utf-8))
                    (restart-pid (sb-ext:process-pid restart-process))
                    (restart-pty (sb-ext:process-pty restart-process))
                    (restart-thread (sb-thread:make-thread (lambda () (restart-program (sb-ext:process-wait restart-process))))))
                    (progn
                        (remhash the-process *process-map*)
                        (push (read-from-pty-stream (getf program-info :pty)) (getf (gethash program-name *uid-map*) :note))
                        (setf   (getf (gethash program-name *uid-map*) :main)
                                restart-process
                                (getf (gethash program-name *uid-map*) :pid)
                                restart-pid
                                (getf (gethash program-name *uid-map*) :pty)
                                restart-pty
                                (getf (gethash program-name *uid-map*) :restart)
                                restart-thread
                                (gethash restart-process *process-map*)
                                program-name)))
            (progn  (remhash the-process *process-map*)
                    (setf (getf (gethash program-name *uid-map*) :restart) nil)
                    (push (read-from-pty-stream (getf program-info :pty)) (getf (gethash program-name *uid-map*) :note))
                    (sb-ext:process-close (getf program-info :main))
                    (setf (getf (gethash program-name *uid-map*) :main) nil)
                    (setf (getf (gethash program-name *uid-map*) :pty) nil)))))

(defun ready-from-pty-stream (the-pty-stream)
    (with-output-to-string (*standard-output*)
        (loop   for the-char = (read-char-no-hang the-pty-stream nil nil)
                while the-char
                do (princ the-char))))

(defun write-to-pty-stream (the-pty-stream the-string)
    (format the-pty-stream the-string)
    (finish-output the-pty-stream))

;
