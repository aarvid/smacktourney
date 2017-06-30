;;;; smacktourney.lisp

(in-package #:smacktourney)

;;; "smacktourney" goes here. Hacks and glory await!

(defparameter *base-dir*
  (asdf:system-source-directory :smacktourney))

(defparameter *users-dir* "users")

(defparameter *user-feebs-dir* "feebs")

(defparameter *feeb-source-dir* "source")

(defparameter *tourney-game-size* 10)

(defparameter *tourney-rounds*  1000)

(defvar *users*  nil )

(defvar *active-tourneys* (make-hash-table :test 'equal))

(defun file-to-string (file)
  (with-open-file (stream file)
   (let ((seq (make-array (file-length stream)
                          :element-type 'character
                          :fill-pointer t)))
     (setf (fill-pointer seq) (read-sequence seq stream))
     seq)))


;; on-lisp ch 4.3
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defun pick-n-breadth (groups n)
  (let ((len (length groups)))
    (loop for i from 0 to (1- n)
          collect (nth (floor i len)
                       (nth (mod i len) groups)))))


(defun decode-planet-name (planet)
  (let ((l (read-from-string (planet-name planet))))
    (setf (second l)
          (parse-timestring (second l)))
    (values-list l)))


(defun save-score-file (planet fb normalized-score)
  (destructuring-bind (user name) (read-from-string (feeb-name fb))
    (multiple-value-bind (tourney timestamp round game-number)
        (decode-planet-name planet)
      (let ((file (feeb-sexp-file tourney user name "log")))
        (with-open-file (s file :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (prin1 (list (format-timestring nil timestamp)
                       round game-number normalized-score)
                 s)
          (terpri s))))))


(defclass tourney ()
  ((name :accessor tourney-name :initform nil :initarg :name)
   (feebs :accessor tourney-feebs :initform nil)
   (timestamp :reader tourney-timestamp :initform (now))
   (rounds :reader tourney-rounds :initform *tourney-rounds* :initarg :rounds)))

(defmethod print-object ((object tourney) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name timestamp) object
      (format stream "~s ~a" name (format-timestring nil timestamp)))))

(defclass tourney-feeb ()
  ((user :accessor tourney-feeb-user :initform nil :initarg :user)
   (name :accessor tourney-feeb-name :initform nil :initarg :name)
   (tourney :reader tourney-feeb-tourney :initform nil :initarg :tourney)
   (last-tourney-timestamp :reader last-tourney-timestamp :initform nil)
   (games :accessor tourney-feeb-games :initform 0 :initarg :games)
   (score :accessor tourney-feeb-score :initform 0 :initarg :score)
   (source :reader tourney-feeb-source :initform nil)
   (smackfeeb :accessor tourney-feeb-smackfeeb :initform nil)))

(defmethod print-object ((object tourney-feeb) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (user name games score) object
      (format stream "~s ~s ~d ~,3f" user name games score))))

(defclass tourney-feeb-source ()
  ((file :reader source-file :initarg :file)
   (text :reader source-text :initarg :text)
   (timestamp :reader source-timestamp :initarg :timestamp)))

(defmethod initialize-instance :after ((tourney tourney) &key)
  (dolist (f (get-tourney-feebs (tourney-name tourney)))
    (push (make-instance 'tourney-feeb
                         :user (first f)
                         :name (second f)
                         :tourney tourney)
          (tourney-feebs tourney))))

(defgeneric directory-tourney (tourney))
(defmethod directory-tourney ((tourney tourney))
  (merge-pathnames
   (make-pathname :directory (list :relative
                                   (tourney-name tourney)))
   *base-dir*))

(defmethod directory-tourney ((tourney string))
  (merge-pathnames
   (make-pathname :directory (list :relative tourney))
   *base-dir*))

(defun directory-tourney-users (tourney)
  (merge-pathnames
   (make-pathname :directory (list :relative *users-dir*))
   (directory-tourney tourney)))


(defun directory-tourney-user (tourney user)
  (merge-pathnames
   (make-pathname :directory (list :relative user))
   (directory-tourney-users tourney)))

(defun directory-tourney-user-feebs (tourney user)
  (merge-pathnames
   (make-pathname :directory (list :relative *user-feebs-dir*))
   (directory-tourney-user tourney user)))

(defun directory-tourney-user-feeb (tourney user feeb)
  (merge-pathnames
   (make-pathname :directory (list :relative feeb))
   (directory-tourney-user-feebs tourney user)))

(defun directory-tourney-user-feeb-source (tourney user feeb)
  (merge-pathnames
   (make-pathname :directory (list :relative *feeb-source-dir*))
   (directory-tourney-user-feeb tourney user feeb)))

(defgeneric directory-feeb (tourney-feeb))
(defmethod directory-feeb ((tourney-feeb tourney-feeb))
  (directory-tourney-user-feeb
     (tourney-feeb-tourney tourney-feeb)
     (tourney-feeb-user tourney-feeb)
     (tourney-feeb-name tourney-feeb)))

(defgeneric directory-feeb-source (tourney-feeb))
(defmethod directory-feeb-source ((tourney-feeb tourney-feeb))
  (directory-tourney-user-feeb-source
     (tourney-feeb-tourney tourney-feeb)
     (tourney-feeb-user tourney-feeb)
     (tourney-feeb-name tourney-feeb)))


(defun get-tourney-users (tourney)
  (let ((path (directory-tourney-users tourney)))
    (mapcar (lambda (p)
              (lastcar (pathname-directory p)))
            (uiop:directory-files path))))

(defun get-tourney-user-feebs (tourney user)
  (mapcar (lambda (p)
            (lastcar (pathname-directory p)))
          (uiop:directory-files (directory-tourney-user-feebs tourney user))))

(defun get-tourney-feebs (tourney)
  (mapcan (lambda (u)
            (mapcar (lambda (f) (list u f)) 
                    (get-tourney-user-feebs tourney u)))
          (get-tourney-users tourney)))

(defgeneric get-feeb-source (tourney-feeb))
(defmethod get-feeb-source ((tourney-feeb tourney-feeb))
  (let ((feeb-dir (directory-feeb-source tourney-feeb)))
    (mapc (lambda (p)
            (push (make-instance 'tourney-feeb-source
                                 :file p
                                 :text (file-to-string p)
                                 :timestamp (universal-to-timestamp
                                             (file-write-date p)))
                  (slot-value tourney-feeb 'source)))
          (uiop:directory-files feeb-dir))))

(defun feeb-source-name-text (tourney user feeb)
  (mapcar (lambda (p)
            (cons (file-namestring p) (file-to-string p)))
          (uiop:directory-files
           (directory-tourney-user-feeb-source tourney user feeb))))

(defun tourney-user-data-file (tourney user)
  (merge-pathnames
   (make-pathname :name "data"
                  :type "sexp")
   (directory-tourney-user tourney user)))


(defun user-feeb-sexp-file (tourney user feeb file-name)
  (merge-pathnames
   (make-pathname :name file-name
                  :type "sexp")
   (directory-tourney-user-feeb tourney user feeb)))

(defun feeb-sexp-file (tourney-feeb file-name)
  (merge-pathnames
   (make-pathname :name file-name
                  :type "sexp")
   (directory-feeb tourney-feeb)))


(defmethod initialize-instance :after ((tourney-feeb tourney-feeb) &key)
  (let ((file (feeb-sexp-file tourney-feeb "data")))
    (if (uiop:file-exists-p file)
        (with-open-file (s file :direction :input)
          (let ((data (read s)))
            (setf (slot-value tourney-feeb 'last-tourney-timestamp)
                  (parse-timestring (first data)))
            (setf (tourney-feeb-games tourney-feeb)
                  (second data))
            (setf (tourney-feeb-score tourney-feeb)
                  (third data)))))
    (get-feeb-source tourney-feeb)))


(defun load-feeb-source (tourney-feeb)
  (let ((smacklisp:*smack-symbols* (feeb-lisp-env
                                    (tourney-feeb-smackfeeb tourney-feeb))))
    (mapc (lambda (src) (smacklisp:load-string (source-text src)))
          (tourney-feeb-source tourney-feeb))))

(defun split-feebs-into-random-groups (tourney size)
  (let* ((groups (group (shuffle (copy-list (tourney-feebs tourney)))
                        size))
         (last-group (lastcar groups))
         (lacking (- size (length last-group))))
    (when (< 0 lacking)
      (when (<= (length groups) 1)
        (error "tourney game size ~a greater than number of entrants ~a"
               size (length last-group)))
      (setf (lastcar groups)
            (append last-group
                    (pick-n-breadth (butlast groups) lacking))))
    groups))

(defun internal-feeb-name (tourney-feeb)
  (prin1-to-string (list (tourney-feeb-user tourney-feeb)
                         (tourney-feeb-name tourney-feeb))))

(defun update-feeb-score (fb normalized-score)
  (setf (tourney-feeb-score fb)
        (/ (+ (* (tourney-feeb-games fb) (tourney-feeb-score fb))
              normalized-score)
           (1+ (tourney-feeb-games fb))))
  (incf (tourney-feeb-games fb)))

(defun process-scores (members)
  (let ((ranking (sort (group-by:group-by members
                                          :key #'(lambda (f)
                                                   (feeb-score
                                                    (tourney-feeb-smackfeeb f)))
                                          :value #'identity)
                       #'> :key #'car))
        (max-pnt (1- (length members))))
    (let ((pnt max-pnt))
     (dolist (gp ranking ranking)
       (let ((normalized-score
               (let ((sum 0)
                     (cnt 0))
                 (dolist (fb (cdr gp) (coerce (/ sum cnt max-pnt) 'double-float))
                   (declare (ignore fb))
                   (incf sum pnt)
                   (decf pnt)
                   (incf cnt)))))
         (dolist (fb (cdr gp))
           (update-feeb-score fb normalized-score)))))))

(defun tourney-game-name (tourney timestamp round game-number)
  (prin1-to-string
   (list tourney
         (format-timestring nil timestamp)
         round
         game-number)))

(defun run-tourney-game (tourney round game-number members)
  (let ((planet (make-instance 'planet
                               :name (tourney-game-name (tourney-name tourney)
                                                        (tourney-timestamp tourney)
                                                        round
                                                        game-number))))
    (dolist (fb members)
      (setf (tourney-feeb-smackfeeb fb)
            (make-feeb planet (internal-feeb-name fb)))
      (load-feeb-source fb))
    (smackfeebs:play planet)
    (process-scores members)
    (dolist (fb members)
      (setf (tourney-feeb-smackfeeb fb) nil))))

(defun run-tourney-round (tourney round)
  (let ((game-number 0))  
    (dolist (feeb-group (split-feebs-into-random-groups tourney
                                                        *tourney-game-size*))
      (run-tourney-game tourney round game-number feeb-group)
      (incf game-number))))

(defun save-scores-to-file (tourney)
  (dolist (fb (tourney-feebs tourney))
    (with-open-file (s
                     (feeb-sexp-file fb "data")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
          (prin1 (list (format-timestring nil
                                          (tourney-timestamp tourney))
                       (tourney-feeb-games fb)
                       (tourney-feeb-score fb))
                 s))))

(defun feeb-changed-p (feeb)
  (or (= 0 (tourney-feeb-games feeb))
      (some
       (lambda (fs)
         (timestamp< (last-tourney-timestamp feeb) (source-timestamp fs))) 
       (tourney-feeb-source feeb))))


(defun changes-to-feebs-p (tourney)
  (some #'feeb-changed-p (tourney-feebs tourney)))

(defun tourney-reset-scores (tourney)
  (mapc (lambda (tourney-feeb)
          (setf (tourney-feeb-games tourney-feeb) 0)
          (setf (tourney-feeb-score tourney-feeb) 0))
        (tourney-feebs tourney)))



(defun run-tourney (tourney-name &optional (rounds *tourney-rounds*))
  (let ((tourney (make-instance 'tourney :name tourney-name :rounds rounds)))
    (setf (gethash tourney-name *active-tourneys*) tourney)
    (unwind-protect
         (progn    
           (when (changes-to-feebs-p tourney)
             (tourney-reset-scores tourney))
           (dotimes (i rounds)
             (run-tourney-round tourney i))
           (save-scores-to-file tourney))
      (setf (gethash tourney-name *active-tourneys*) nil))))

(defun tourney-ranking-list (tourney-name)
  (let* ((tourney (make-instance 'tourney :name tourney-name))
         (ranking (sort (copy-list (tourney-feebs tourney))
                        #'> :key #'tourney-feeb-score)))
    (mapcar (lambda (f)
              (list (tourney-feeb-user f)
                    (tourney-feeb-name f)
                    (tourney-feeb-games f)
                    (tourney-feeb-score f)))
            ranking)))


(defun print-tourney-ranking (tourney-name)
  (let* ((tourney (make-instance 'tourney :name tourney-name))
         (ranking (sort (copy-list (tourney-feebs tourney))
                        #'> :key #'tourney-feeb-score))
         (max-user 0)
         (max-name 0)
         (max-games 0))
    (dolist (fb ranking)
      (setf max-user (max max-user (length (tourney-feeb-user fb))))
      (setf max-name (max max-name (length (tourney-feeb-name fb))))
      (setf max-games (max max-games (tourney-feeb-games fb))))
    (setf max-games (ceiling (log (1+ max-games) 10)))
    (dolist (fb ranking)
      (format t "~va ~va ~vd ~,3f~%"
              max-user
              (tourney-feeb-user fb)
              max-name
              (tourney-feeb-name fb)
              max-games
              (tourney-feeb-games fb)
              (tourney-feeb-score fb)))))


(defun create-tourney (name)
  (when (uiop:directory-exists-p (directory-tourney name))
    (error "Tourney ~a already exists" name))
  (ensure-directories-exist (directory-tourney-users name)))

(defun create-tourney-user (tourney name pwd email)
  (unless (uiop:directory-exists-p (directory-tourney tourney))
    (error "Tourney ~a does not exist" tourney))
  (ensure-directories-exist (directory-tourney-user tourney name))
  (ensure-directories-exist (directory-tourney-user-feebs tourney name))
  (let ((alist (list (cons :password
                           (ironclad:pbkdf2-hash-password-to-combined-string
                            (babel:string-to-octets pwd)))
                     (cons :email email))))
    (with-open-file (s (tourney-user-data-file tourney name)
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (prin1 alist s))
    alist))

(defun create-tourney-feeb (tourney user feeb )
  (unless (uiop:directory-exists-p (directory-tourney tourney))
    (error "Tourney ~a does not exist" tourney))
  (unless (uiop:directory-exists-p (directory-tourney-user tourney user))
    (error "Tourney user ~a does not exist" user))
  (ensure-directories-exist (directory-tourney-user-feeb tourney user feeb))
  (ensure-directories-exist (directory-tourney-user-feeb-source
                             tourney user feeb)))

(defun check-tourney-user-password (tourney user password)
  (handler-case
      (when (uiop:directory-exists-p (directory-tourney-user tourney user))
        (with-open-file (s (tourney-user-data-file tourney user)
                           :direction :input
                           :if-does-not-exist :error)
          (let ((hash (cdr (assoc :password (read s)))))
            (ironclad:pbkdf2-check-password (babel:string-to-octets password)
                                            hash))))         
    (condition () nil)))

(defun tourney-user-exists-p (tourney user )
  (unless (uiop:directory-exists-p (directory-tourney tourney))
    (error "Tourney ~a does not exist" tourney))
  (uiop:directory-exists-p (directory-tourney-user tourney user)))

(defun tourney-feeb-exists-p (tourney user feeb)
  (unless (uiop:directory-exists-p (directory-tourney tourney))
    (error "Tourney ~a does not exist" tourney))
  (uiop:directory-exists-p (directory-tourney-user-feeb tourney user feeb)))

(defun valid-user-name-p (user)
  (and (< 0 (length user))
       (<= (length user) 25)
       (not (find-if #'(lambda (c)
                         (or (not (standard-char-p c))
                             (char= c #\Space)
                             (char= c #\/)))
                     user))))

(defun valid-feeb-name-p (feeb)
  (valid-user-name-p feeb))

(defun upload-feeb-source (tourney user feeb source filename)
  (let ((src-dir (directory-tourney-user-feeb-source tourney user feeb)))
    (ensure-directories-exist src-dir)
    (dolist (f (uiop:directory-files src-dir))
      (unless (uiop:directory-pathname-p f)
        (delete-file f)))
    (copy-file source
               (merge-pathnames (parse-namestring filename)
                                (directory-tourney-user-feeb-source
                                 tourney user feeb)))))

(defun expunge-tourney-user-feeb (tourney user feeb)
  (unless (uiop:directory-exists-p (directory-tourney tourney))
    (error "Tourney ~a does not exist" tourney))
  (unless (tourney-user-exists-p  tourney user)
    (error "Tourney ~a, User ~a does not exist" tourney user))
  (unless (tourney-feeb-exists-p tourney user feeb)
    (error "Tourney ~a, User ~a, Feeb ~a does not exist" tourney user feeb))
  (uiop:delete-directory-tree (directory-tourney-user-feeb tourney user feeb)
                              :if-does-not-exist :error
                              :validate t))

(defun expunge-tourney-user (tourney user)
  (unless (uiop:directory-exists-p (directory-tourney tourney))
    (error "Tourney ~a does not exist" tourney))
  (unless (tourney-user-exists-p  tourney user)
    (error "Tourney ~a, User ~a does not exist" tourney user))
  (mapc (lambda (feeb)
          (expunge-tourney-user-feeb tourney user feeb))
        (get-tourney-user-feebs tourney user))
  (uiop:delete-directory-tree (directory-tourney-user tourney user)
                              :if-does-not-exist :error
                              :validate t))


(defun change-user-password (tourney user pwd)
  "will create password if it does not exist."
  (when (uiop:directory-exists-p (directory-tourney-user tourney user))
    (let* ((data-file (tourney-user-data-file tourney user))
           (data (with-open-file (s data-file
                                    :direction :input
                                    :if-does-not-exist :create)
                   (read s nil nil)))
           (hash (ironclad:pbkdf2-hash-password-to-combined-string
                  (babel:string-to-octets pwd))))
      (if (assoc :password data)
          (setf (cdr (assoc :password data)) hash)
          (push (cons :password hash) data))
      (with-open-file (s data-file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (prin1 data s)))))


(defun tourney-timer-event (tourney)
  (unless (gethash tourney *active-tourneys*)
    (run-tourney tourney 1)))

(defun get-tourney-timer (tourney)
  (find-if 
   (lambda (timer)
             (string-equal tourney (trivial-timers:timer-name timer)))
   (trivial-timers:list-all-timers)))


(defun start-tourney-timer (tourney)
  (let ((timer (or (get-tourney-timer tourney)
                   (trivial-timers:make-timer (lambda ()
                                                (tourney-timer-event tourney))
                                              :name tourney))))
   (trivial-timers:schedule-timer timer 10 :repeat-interval 10 :absolute-p nil)))


(defun stop-tourney-timer (tourney)
  (when-let ((timer (get-tourney-timer tourney)))
    (trivial-timers:unschedule-timer timer)))

(defun tourney-timer-active-p (tourney)
  (when-let ((timer (get-tourney-timer tourney)))
    (trivial-timers:timer-scheduled-p timer)))


(defun feeb-source-open-p (tourney user feeb)
  (let ((file (user-feeb-sexp-file tourney user feeb "config")))
    (when (uiop:file-exists-p file)
      (with-open-file (s file :direction :input)
        (assoc :open-p (read s))))))

(defun set-feeb-open-source (tourney user feeb open-p)
  (let* ((file (user-feeb-sexp-file tourney user feeb "config"))
         (data (with-open-file (s file
                                  :direction :input
                                  :if-does-not-exist :create)
                 (read s nil nil))))
    (if (assoc :open-p data)
        (setf (cdr (assoc :open-p data)) open-p)
        (push (cons :open-p open-p) data))
    (with-open-file (s file
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (prin1 data s))))
