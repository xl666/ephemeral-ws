

(if (not (boundp 'proyectos-workspaces-hash))
    (setq proyectos-workspaces-hash (make-hash-table :test 'equal)))

(defun limpiar-hash (tabla workspace)
  (maphash (lambda (key val)
	     (if (string= workspace val)
		 (puthash key nil tabla)))
	   tabla))

(defun nombre-buff(buffer)
  (if (and (not (string-match-p "*temp*" (buffer-name)))
	   (not (string-match-p "*helm*" (buffer-name)))
	   (not (string-match-p "*mu4e*" (buffer-name)))
	   (not (string-match-p "magit" (buffer-name)))
	   (not (string-match-p "*Bufler*" (buffer-name))))
      (if (and (boundp 'detached) detached)
	  (setq-local bufler-workspace-name nil)
	(if (not bufler-workspace-name)
	    (let* ((wr (gethash (get-proyectname-buffer (current-buffer)) proyectos-workspaces-hash)))
	      
	      (if wr
		  (progn
		    (set (make-local-variable 'bufler-workspace-name) wr)
		    (setf bufler-cache nil)
		    (force-mode-line-update 'all))
		)))))
  buffer)



 
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


(defun cambiar-proyecto ()
  (helm-projectile-switch-project)
  (switch-to-buffer (car (buffer-list))))

(defun seleccionar-proyecto-and-buffercentral ()
  (cambiar-proyecto)
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


(setq major-modes-ignorados '("treemacs-mode"))


(defun quitar-buffs (lista &optional res)
  (if (not lista)
      res
    (if (member (symbol-name (buffer-local-value 'major-mode (car lista)))
		major-modes-ignorados)
	(quitar-buffs (cdr lista) res)
      (quitar-buffs (cdr lista)
		    (append res (list (car lista)))))))


(defun mayor-mode-mas-largo (lista-buffs)
  (let ((count 0))
    (while lista-buffs
      (progn
	(if (> (length (quitar-major-mode (symbol-name (buffer-local-value 'major-mode (car lista-buffs)))))
	       count)
	    (setq count (length (symbol-name (buffer-local-value 'major-mode (car lista-buffs))))))
	(setq lista-buffs (cdr lista-buffs))))
    count))

(defun generate-spaces-string (len)
  (let ((cadena "")
	(count 0))
    (while (< count len)
      (progn
	(setq cadena (concat cadena " "))
	(setq count (+ count 1))))
    cadena))


(defun add-padding-string (cadena longitud_deseada)
  (if (>= (length cadena) longitud_deseada)
      cadena
    (concat cadena
	    (generate-spaces-string (- longitud_deseada
				       (length cadena))))))

(defun label-buffers (lista-buffs longitud-mayor-mode &optional res tabla)
  "Adds a description to buffer based on properties and returns a hashmap description->buffer"
  (if (not lista-buffs)
      res
    (let* ((llave (concat (add-padding-string
			   (quitar-major-mode (symbol-name (buffer-local-value 'major-mode (car lista-buffs))))
			   longitud-mayor-mode)
			  "»»   "
		   (buffer-name (car lista-buffs)))))

      (progn (puthash llave (car lista-buffs) tabla)
	     (label-buffers
	      (cdr lista-buffs)
	      longitud-mayor-mode
	      (append res (list llave))
	      tabla)))))

(defun quitar-major-mode (buff-major-mode)
  (if (s-ends-with-p "-mode" buff-major-mode)
      (substring buff-major-mode 0 -5)
    buff-major-mode))

(defun tabla-buffers ()
  "Para facilitar la creación de tabla hash"
  (let* ((tabla (make-hash-table :test 'equal))
	 (lista-buffs (quitar-buffs (listar-en-workspace)))
	 (llaves (label-buffers 
		  (if (eq (car lista-buffs) (current-buffer))
		      (cdr lista-buffs)
		    
		    lista-buffs)
		  (mayor-mode-mas-largo lista-buffs)
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
	     (setq-local cacheado nil)
	     (setf bufler-cache nil)
	     (force-mode-line-update 'all)
	     (message "Buffer detached"))))


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
	   (treemacs-do-remove-workspace workspace) ; try to remove if exists, prevetns a bug
	   (treemacs-do-create-workspace workspace)
	   (mi-treemacs-do-switch-workspace workspace)
	   (treemacs-add-project-to-workspace path-proyecto)
	   (borrar-cache))))


(defun matar-buffers (buffers)
  (mapc (lambda (buffer)
	  (kill-buffer buffer))
	buffers))

(defun borrar-workspace ()
  (interactive)
  (let* ((workspace (car (frame-parameter nil 'bufler-workspace-path))))    
    (if workspace
	(progn (treemacs-do-remove-workspace workspace)
	       (limpiar-hash proyectos-workspaces-hash workspace)
	       (matar-buffers (listar-en-workspace))
	       (exwm-workspace-delete)))))


(defun listar-workspaces (tabla)
  (let ((resultado nil))
    (progn (maphash (lambda (key val)
		      (if val
			  (setq resultado (append resultado (list val)))))
		    tabla)
	   resultado)))

(defun get-frame-from-workspace (workspace)
  (car (mapcan (lambda (frame)
		 (if (string=
		      (car (frame-parameter frame 'bufler-workspace-path))
		      workspace)
		     (list frame)))
	       (frame-list))))


(defun quitar-ws-lista (ws lista-ws)
  (mapcan (lambda (x)
	    (if (not (string=
		      x ws))
		(list x)))
	  lista-ws))


(defun ephemeral-ws-switch-workspace ()
  (interactive)
  (let* ((ws (completing-read "Workspace to switch: "
			      (if (car (frame-parameter nil 'bufler-workspace-path))
				  (cons "*Default*"
					(quitar-ws-lista
				   (car (frame-parameter nil 'bufler-workspace-path))
				   (listar-workspaces
				    proyectos-workspaces-hash)))
				(listar-workspaces
				    proyectos-workspaces-hash))))
	 (frame (get-frame-from-workspace ws)))
    (if frame
	(exwm-workspace-switch frame)
      (exwm-workspace-switch 0))))


(advice-add 'generate-new-buffer :filter-return #'nombre-buff)
; (advice-remove 'generate-new-buffer #'nombre-buff)
