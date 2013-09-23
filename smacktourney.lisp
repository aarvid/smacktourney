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
          collect (nth (floor i len) (nth (mod i len) groups) ))))






(defun decode-planet-name (planet)
  (let ((l (read-from-string (planet-name planet))))
    (setf (second l)
          (local-time:parse-timestring (second l)))
    (values-list l)))






(defun save-score-file (planet fb normalized-score)
  (destructuring-bind (user name) (read-from-string (feeb-name fb))
    (multiple-value-bind (tourney timestamp round game-number)
        (decode-planet-name planet)
      (let ((file (feeb-sexp-file tourney user name "log")))
        (with-open-file (s file :direction :output
                                :if-exists :append
                                :if-does-not-exist :create)
          (prin1 (list (local-time:format-timestring nil timestamp)
                       round game-number normalized-score)
                 s)
          (terpri s))))))













(defclass tourney ()
  ((name :accessor tourney-name :initform nil :initarg :name)
   (feebs :accessor tourney-feebs :initform nil)
   (timestamp :reader tourney-timestamp :initform (local-time:now))
   (rounds :reader tourney-rounds :initform *tourney-rounds* :initarg :rounds)))

(defmethod print-object ((object tourney) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name timestamp) object
      (format stream "~s ~a" name (local-time:format-timestring nil timestamp)))))

(defclass tourney-feeb ()
  ((user :accessor tourney-feeb-user :initform nil :initarg :user)
   (name :accessor tourney-feeb-name :initform nil :initarg :name)
   (tourney :reader tourney-feeb-tourney :initform nil :initarg :tourney)
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


(defmethod directory-feeb ((tourney-feeb tourney-feeb))
  (merge-pathnames
   (make-pathname :directory (list :relative
                                   (tourney-feeb-name tourney-feeb)))
   (directory-tourney-user-feebs (tourney-feeb-tourney tourney-feeb)
                                 (tourney-feeb-user tourney-feeb))))

(defmethod directory-feeb-source ((tourney-feeb tourney-feeb))
  (merge-pathnames
   (make-pathname :directory (list :relative *feeb-source-dir*))
   (feeb-directory tourney-feeb)))


(defun get-tourney-users (tourney)
  (let ((path (directory-tourney-users tourney)))
    (mapcar (lambda (p)
              (lastcar (pathname-directory p)))
            (list-directory path))))

(defun get-tourney-feebs (tourney)
  (mapcan (lambda (u)
            (let ((path (directory-tourney-user-feebs tourney u)))
              (mapcar (lambda (p)
                        (list u (lastcar (pathname-directory p))))
                      (list-directory path))))
          (get-tourney-users tourney)))

(defmethod get-feeb-source ((tourney-feeb tourney-feeb))
  (let ((feeb-dir (directory-feeb-source tourney-feeb)))
    (mapc (lambda (p)
            (push (make-instance 'tourney-feeb-source
                                 :file p
                                 :text (file-to-string p)
                                 :timestamp (local-time:universal-to-timestamp
                                             (file-write-date p)))
                  (slot-value tourney-feeb 'source)))
          (list-directory feeb-dir))))

(defun tourney-user-data-file (tourney user)
  (merge-pathnames
   (make-pathname :name "data"
                  :type "sexp")
   (directory-tourney-user tourney user)))

(defun feeb-sexp-file (tourney-feeb file-name)
  (merge-pathnames
   (make-pathname :name file-name
                  :type "sexp")
   (directory-feeb tourney-feeb)))


(defmethod initialize-instance :after ((tourney-feeb tourney-feeb) &key)
  (let ((file (feeb-sexp-file tourney-feeb "data")))
    (when (fad:file-exists-p file)
      (with-open-file (s file :direction :input)
        (let ((data (read s)))
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
         (local-time:format-timestring nil timestamp)
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
          (prin1 (list (local-time:format-timestring nil (tourney-timestamp tourney))
                       (tourney-feeb-games fb)
                       (tourney-feeb-score fb))
                 s))))

(defun run-tourney (tourney-name &optional (rounds *tourney-rounds*))
  (let ((tourney (make-instance 'tourney :name tourney-name :rounds rounds)))
    (dotimes (i rounds)
      (run-tourney-round tourney i))
    (save-scores-to-file tourney)))


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
  (when (directory-exists-p (directory-tourney name))
    (error "Tourney ~a already exists" name))
  (ensure-directories-exist (directory-tourney-users name)))

(defun create-tourney-user (tourney name pwd email)
  (unless (directory-exists-p (directory-tourney tourney))
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

(defun check-tourney-user-password (tourney user password)
 (when (directory-exists-p (directory-tourney-user tourney user))
   (with-open-file (s (tourney-user-data-file tourney user)
                      :direction :input
                      :if-does-not-exist :create)
     (let ((hash (cdr (assoc :password (read s)))))
       (ironclad:pbkdf2-check-password (babel:string-to-octets password)
                                       hash)))))
