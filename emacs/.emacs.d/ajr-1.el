(defun ajr-shelllike-filter (proc string)
  (let* ((buffer (process-buffer proc))
	 (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (if (not (mark)) (push-mark))
      (exchange-point-and-mark) ;Use the mark to represent the cursor location
      (dolist (char (append string nil))
	(cond ((char-equal char ?\r)
	       (move-beginning-of-line 1))
	      ((char-equal char ?\n)
	       (move-end-of-line 1) (newline))
	      (t
	       (if (/= (point) (point-max)) ;Overwrite character
		   (delete-char 1))
	       (insert char))))
      (exchange-point-and-mark))
    (if window
	(with-selected-window window
	  (goto-char (point-max))))))

(defun ajr-start-process-in-buffer (buffer-name
				    program-name
				    program
				    &optional program-dir
				    initial-message
				    &rest program-args)
  "Starts a process in buffer with BUFFER-NAME. If an optional
INITIAL-MESSAGE is supplied it will be added to the beginning of the
buffer. If you pass the optional PROGRAM-DIR this will change to the
argument directory before starting the program. The final arugments
will be pass as PROGRAM-ARGS to the PROGRAM."
  (with-current-buffer (generate-new-buffer buffer-name)
    (special-mode)
    (setq buffer-read-only nil)
    (when initial-message
      (insert initial-message)
      (newline))
    (when program-dir
      (cd program-dir))
    (let ((proc (apply 'start-process
		       (append
			(list
			 program-name
			 (current-buffer)
			 program)
			program-args))))
      (set-process-filter proc 'ajr-shelllike-filter)
      (pop-to-buffer (current-buffer)))))

(defun ajr-org-basic-capture (key name filename)
  "Generates a basic capture template. It will
  prompt you for the title of the heading and place
  the point in the body of the heading. KEY is the
  character that needs to be pressed in org capture
  to use this template. NAME is the name of the capture
  template. FILENAME is which org file in your org-directory."
  (list key name 'entry
	(list 'file+headline filename name)
	"* %^{Title?}\n%?"))

(defun ajr-search-web (term)
  "Prompts the user for a search TERM. Searches
  duckduckgo with eww for the term. Inspired by
  https://gist.github.com/brenns10/69d39f6c46170093f73d"
  (interactive "MSearch Web: ")
  (eww (format "https://html.duckduckgo.com/html/?q=%s"
	       (url-hexify-string term))))

(defcustom ajr-sync-mail-program
  "syncmail"
  "Program that syncs mail and updates notmuch")

(defun ajr-sync-mail ()
  "Starts a process name `syncmail' and sends the output to
  a special mode buffer. This will pop to the buffer as the
  process is running."
  (interactive)
  (ajr-start-process-in-buffer
   "*syncmail*"
   "syncmail"
   ajr-sync-mail-program))

(defcustom ajr-music-dir
  (concat (getenv "HOME")
	  "/music")
  "Directory where your music is kept.")

(defconst ajr--no-dot-regexp
  "^[^\\.].+$"
  "Only matches files that do not start with a dot")

(defun ajr--dir-subdirs (directory)
  "Returns the list of sub dirs inside the argument DIRECTORY.
  Ignores directories that start with a `.' "
  (mapcar 'car (seq-filter (lambda (f-or-d)
			     (not (eq (nth 1 f-or-d)
				      nil)))
			   (directory-files-and-attributes directory
							   nil
							   ajr--no-dot-regexp))))

(defun ajr--all-albums ()
  "Returns a list of pairs of all available albums `(ARTIST . ALBUM)'"
  (let ((artists (ajr--dir-subdirs ajr-music-dir)))
    (mapcan (lambda (artist)
	      (let ((artist-path (string-join
				  (list ajr-music-dir
					"/" artist))))
		(mapcar (lambda (album)
			  `(,artist . ,album))

			(ajr--dir-subdirs artist-path))))
	    artists)))

(defun ajr--ask-album ()
  "Prompts the user to choose an album, returns `(ARTIST . ALBUM)"
  (let* ((albums (ajr--all-albums))
	 (album-display-names
	  (mapcar (lambda (album)
		    (format "%s - %s" (car album) (cdr album)))
		  albums))
	 (albums-alist (mapcar (lambda (n)
				 `(,(nth n album-display-names) .
				   ,(nth n albums)))
			       (number-sequence 0 (- (length albums) 1)))))

    (cdr (assoc (completing-read "Which album? "
				 album-display-names)
		albums-alist))))

(defun ajr--album-path (album)
  "Gets the path of an album from `(ARTIST . ALBUM)'"
  (string-join (list ajr-music-dir
		     "/"
		     (car album)
		     "/"
		     (cdr album))))

(defun ajr-bongo-play-album ()
  "Prompts the user for an album and starts playing it"
  (interactive)
  (let ((album (ajr--ask-album)))
    (with-bongo-playlist-buffer
      (bongo-stop)
      (bongo-erase-buffer)
      (bongo-insert-directory-tree (ajr--album-path album))
      (goto-char (point-min))
      (bongo-play))))

(defcustom ajr-podcast-dir
  (concat (getenv "HOME")
	  "/podcasts")
  "Directory where your podcasts are kept.
  Used the `ajr-podcast-*' functions.")

(defun ajr-podcast-dired ()
  "Opens dired buffer to `ajr-podcast-dir' in other window"
  (interactive)
  (find-file-other-window ajr-podcast-dir))

(defcustom ajr-video-dir
  (concat (getenv "HOME")
	  "/videos")
  "Directory where your videos are kept.
  Used the `ajr-video-*' functions."
  :type 'directory)

(defcustom ajr-video-program
  "mpv"
  "Program used to play videos.
  This program should accept the path to the video as its argument."
  :type 'string)

(defcustom ajr-video-regexp
  ".+\\.\\(mp4\\|webm\\|mkv\\)$"
  "Only matches files ending in `mp4' or `webm' or `mkv'."
  :type 'regexp)

(defun ajr--ask-video (is-by-date video-dir)
  (let ((videos (directory-files
		 video-dir
		 nil
		 ajr-video-regexp))
	(sorted-videos (mapcar 'car (sort
				     (directory-files-and-attributes
				      video-dir
				      nil
				      ajr-video-regexp)
				     (lambda (x y)
				       (time-less-p
					(file-attribute-modification-time (cdr y))
					(file-attribute-modification-time (cdr x))))))))

    (completing-read "Which video? " (if is-by-date
					 sorted-videos
				       videos))))

(defun ajr-video-play (arg &optional video-dir)
  "Prompts the user for a video from `ajr-video-dir'. If you pass a
`video-dir' in it will use that instead of `ajr-video-dir'. Uses the
`ajr-video-program' to play the video. Use C-u to sort the videos by
date (newest first)."
  (interactive "P")
  (let* ((vdir (or video-dir ajr-video-dir))
	 (video (ajr--ask-video arg vdir))
	 (video-buffer (get-buffer-create "*video-player*"))
	 (script-proc-buffer
	  (make-comint-in-buffer "video-player"
				 video-buffer
				 ajr-video-program
				 nil
				 (string-join (list vdir
						    "/"
						    video))))
	 (video-proc (get-buffer-process video-buffer)))
    (with-current-buffer video-buffer
      ;; If the buffer was previously in special mode,
      ;; need to set read only to false
      (setq buffer-read-only nil))
    (set-process-sentinel video-proc
			  (lambda (proc change)
			    (with-current-buffer (process-buffer proc)
			      (special-mode))))))

(defun ajr-video-dired ()
  "Opens dired buffer to `ajr-video-dir' in other window"
  (interactive)
  (find-file-other-window ajr-video-dir))

(require 'url-util)
(defun ajr-video-youtube-dl-at-point ()
  (interactive)
  (let ((yt-url (url-get-url-at-point)))
    (ajr-start-process-in-buffer
     "*youtube-dl*"
     (format "youtube-dl %s" yt-url)
     "youtube-dl"
     ajr-video-dir
     yt-url
     "-f"
     "best[height<=1080]"
     yt-url)))

(defvar ajr-mini-scroll-amount 5
  "Scroll lines used by ajr-mini-scroll.")

(defvar ajr-mini-scroll-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<down>") 'ajr-mini-scroll-up)
    (define-key m (kbd "<up>") 'ajr-mini-scroll-down)
    m))

(defun ajr-mini-scroll (lines)
  "Scroll by `lines' lines"
  (interactive)
  (scroll-up lines)
  (set-transient-map ajr-mini-scroll-map))

(defun ajr-mini-scroll-down ()
  "Scroll down"
  (interactive)
  (ajr-mini-scroll (- ajr-mini-scroll-amount)))

(defun ajr-mini-scroll-up ()
  "Scroll up"
  (interactive)
  (ajr-mini-scroll ajr-mini-scroll-amount))

(defun ajr-ask-before-closing ()
  "Close frame only if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Do you want to close this frame? "))
      (save-buffers-kill-terminal)
    (message "Ok")))

(defun ajr-scratch ()
  "Switch to the scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun ajr-org-goto-first-heading()
  (goto-char (point-min))
  (unless (org-at-heading-p)
    (org-next-visible-heading 1)))

(defun ajr-org-add-edit-timestamp ()
  (save-excursion
    (ajr-org-goto-first-heading)
    (if (eq (line-number-at-pos) 1)
	(progn
	  (newline)
	  (previous-line))
      (progn
	(previous-line)
	(if (org-at-timestamp-p)
	    (kill-whole-line)
	  (next-line))
	(newline)
	(previous-line)))
    (org-insert-time-stamp (current-time))))

(defun ajr-android-screenshot ()
  "Uses adb to capture a screencap of the connected device.
The screenshot is opened in the other window."
  (interactive)
  (let ((fname (expand-file-name
		(concat
		 "~/tmp/android-"
		 (format-time-string "%Y%m%d%H%M%S")
		 ".png"))))
    (shell-command
     (concat
      "adb exec-out screencap -p > "
      fname))
    (find-file-other-window fname)))