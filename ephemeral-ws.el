
(progn (treemacs) (treemacs)) ;; asegurarse de que treemacs cargue


(defun asociar-buffer-workspace ()
  (interactive)
  (call-interactively #'bufler-workspace-buffer-name-workspace)
  (setq-local detached nil)
  (setq-local cacheado nil))


(defun get-buffer-workspace (buffer)
  (buffer-local-value 'bufler-workspace-name buffer))


(defun get-proyectname-buffer (buffer)
  (let* ((ruta (buffer-local-value 'default-directory (current-buffer))))
    (if ruta
	(let* ((raiz (projectile-project-root ruta)))
	  (if raiz
	      (projectile-project-name raiz))))
    ))

(defun get-proyect-raiz-from-buffer (buffer)
  (let* ((ruta (buffer-local-value 'default-directory (current-buffer))))
    (if ruta
	(let* ((raiz (projectile-project-root ruta)))
	  (if raiz
	      raiz)))
    ))

(defun listar-en-workspace ()
  "Devuelve una lista de buffers con el mismo workspace del frame"
  (let ((current-ws (car(frame-parameter nil 'bufler-workspace-path))))
    (if current-ws
	(mapcan (lambda (x)
		       (if (string= current-ws (get-buffer-workspace x))
			   (list x)))
		(buffer-list)))))



(defun seleccionar-proyecto ()
   (let* (
    (proyecto (completing-read "Projectile project: "
		     (cons (projectile-project-name) (sort (mapcar #'projectile-project-name (projectile-relevant-known-projects)) #'string<)))))
     proyecto))

(defun seleccionar-proyecto-path ()
   (let* (
    (proyecto (completing-read "Projectile project: "
		     (projectile-relevant-known-projects))))
     proyecto))

(defun seleccionar-proyecto-and-buffercentral ()
  (helm-projectile-switch-project)
  ;(projectile-switch-project-by-name path)
  (get-proyect-raiz-from-buffer (current-buffer)))

(defun seleccionar-workspace ()
  (let* (
    (workspace (completing-read "Bufler workspace: "
                                        (seq-uniq
                                         (cl-loop for buffer in (buffer-list)
                                                  when (buffer-local-value 'bufler-workspace-name buffer)
                                                  collect it)))))
     workspace))

(defun seleccionar-proyecto-workspace ()
  (let* ((proyecto (seleccionar-proyecto-path))
	 (workspace (seleccionar-workspace)))
     `(,proyecto ,workspace)))



(defun borrar-cache ()
  (mapcar (lambda (x)
	    (with-current-buffer x
	      (if (boundp 'cacheado)
		  (progn (setq cacheado nil))
		)))
	  (buffer-list)))

(defun centaur-tabs-buffer-groups ()
  (if (and (boundp 'cacheado) cacheado)
      (list cacheado)
    (if bufler-workspace-name
	(progn (set (make-local-variable 'cacheado) bufler-workspace-name)
	       (list bufler-workspace-name))

      (if (and (boundp 'detached) detached)
	  (progn
	    (set (make-local-variable 'cacheado) "Emacs")
	    (list "Emacs"))
	(let* ((wr (gethash (get-proyectname-buffer (current-buffer)) proyectos-workspaces-hash)))
	  (if wr
	      (progn (set (make-local-variable 'cacheado) wr)
		     (set (make-local-variable 'bufler-workspace-name) wr)
					;(setq-local bufler-workspace-name wr) ;; se propaga de forma rara
		     (list wr))
	    (progn
	      (set (make-local-variable 'cacheado) "Emacs")
	      (list "Emacs"))))))))

(setq major-modes-ignorados '("treemacs-mode"))


(defun quitar-buffs (lista &optional res)
  (if (not lista)
      res
    (if (member (symbol-name (buffer-local-value 'major-mode (car lista)))
		major-modes-ignorados)
	(quitar-buffs (cdr lista) res)
      (quitar-buffs (cdr lista)
		    (append res (list (car lista)))))))


(defun label-buffers (lista-buffs &optional res tabla)
  "Adds a description to buffer based on properties and returns a hashmap description->buffer"
  (if (not lista-buffs)
      res
    (let* ((llave (concat (symbol-name (buffer-local-value 'major-mode (car lista-buffs)))
			  "\t »» \t"
		   (buffer-name (car lista-buffs)))))

      (progn (puthash llave (car lista-buffs) tabla)
	     (label-buffers
	      (cdr lista-buffs)
	      (append res (list llave))
	      tabla)))))

(defun tabla-buffers ()
  "Para facilitar la creación de tabla hash"
  (let* ((tabla (make-hash-table :test 'equal))
	 (lista-buffs (quitar-buffs (listar-en-workspace)))
	 (llaves (label-buffers
		  (if (eq (car lista-buffs) (current-buffer))
		      (cdr lista-buffs)
		    lista-buffs)
		  nil tabla)))
	  `(,llaves ,tabla)))

(defun seleccionar-buffer-workspace ()
  (interactive)
  (let ((current-ws (car (frame-parameter nil 'bufler-workspace-path))))
    (if (not current-ws)
	(call-interactively #'bufler-switch-buffer)
      
      (let* ((todo (tabla-buffers))
	     (llaves (first todo))
	     (tabla (second todo))
	     (label (completing-read "Buffer: " llaves)))
	(switch-to-buffer (gethash label tabla))))))


(if (not (boundp 'proyectos-workspaces-hash))
    (setq proyectos-workspaces-hash (make-hash-table :test 'equal)))

(defun crear-asociacion (proyecto workspace)
  (puthash proyecto workspace proyectos-workspaces-hash))

(defun asociar-proyecto-workspace ()
  (interactive)
  (let* ((todo (seleccionar-proyecto-workspace))
	 (proyecto (first todo))
	 (workspace (second todo)))
    (progn (puthash (projectile-project-name proyecto)
	    workspace proyectos-workspaces-hash)
	   (treemacs-add-project-to-workspace proyecto)
	   (borrar-cache))))




;; mitigar problema de buffers *temp*

(defun centaur-tabs-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Centaur-Tabs mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (if (string-match-p "*temp*" (buffer-name))
      nil
    (and (eq (eval centaur-tabs-display-line-format) centaur-tabs-header-line-format)
	 (eq centaur-tabs-current-tabset-function 'centaur-tabs-buffer-tabs)
	 (eq (current-buffer) (window-buffer (selected-window)))
	 (let ((bl (centaur-tabs-tab-values (centaur-tabs-current-tabset)))
	       (b  (current-buffer))
	       found sibling)
	   (while (and bl (not found))
	     (if (eq b (car bl))
		 (setq found t)
	       (setq sibling (car bl)))
	     (setq bl (cdr bl)))
	   (when (and (setq sibling (or (car bl) sibling))
		      (buffer-live-p sibling))
	     ;; Move sibling buffer in front of the buffer list.
	     (save-current-buffer
	       (switch-to-buffer sibling)))))))


(defun centaur-tabs-on-modifying-buffer ()
  "Function to be run after the buffer is first changed."
  (if (string-match-p "*temp*" (buffer-name))
      nil
    (progn
      (set-buffer-modified-p (buffer-modified-p))
      (centaur-tabs-set-template centaur-tabs-current-tabset nil)
      (centaur-tabs-display-update))))


; integración de treemacs


(defun mi-treemacs-do-switch-workspace (name)
  "Switch to a new workspace.
Return values may be as follows:

* If there are no workspaces to switch to:
  - the symbol `only-one-workspace'
* If everything went well:
  - the symbol `success'
  - the selected workspace"
  (treemacs--maybe-load-workspaces)
  (treemacs-block
   (treemacs-return-if (= 1 (length treemacs--workspaces))
     'only-one-workspace)
   (let* ((workspaces (->> treemacs--workspaces
                           (--reject (eq it (treemacs-current-workspace)))
                           (--map (cons (treemacs-workspace->name it) it))))
          (selected (cdr (--first (string= (car it) name) workspaces))))
     (setf (treemacs-current-workspace) selected)
     (treemacs--invalidate-buffer-project-cache)
     (treemacs--rerender-after-workspace-change)
     (run-hooks 'treemacs-switch-workspace-hook)
     (treemacs-return
      `(success ,selected)))))


(defun detach-buffer ()
  "Quitar el buffer de workspace"
  (interactive)
  (if bufler-workspace-name
      (progn (setq-local bufler-workspace-name nil)
	     (set (make-local-variable 'detached) 1)
	     (setq-local cacheado nil))))


;; integración completa

(defun crear-workspace ()
  (interactive)
  (let* ((path-proyecto (seleccionar-proyecto-and-buffercentral))
	 (proyecto (projectile-project-name path-proyecto))
	 (workspace (seleccionar-workspace)))
    (progn (exwm-workspace-add)
	   (crear-asociacion proyecto workspace)
	   (set (make-local-variable 'bufler-workspace-name) workspace)
	   (bufler-workspace-frame-set `(,workspace))
	   (treemacs-do-create-workspace workspace)
	   (mi-treemacs-do-switch-workspace workspace)
	   (treemacs-add-project-to-workspace path-proyecto)
	   (borrar-cache))))

