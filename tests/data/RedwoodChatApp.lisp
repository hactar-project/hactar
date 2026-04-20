(target :redwood)

(in-file "src/style.css")
(defcss chat-styles
  (:root
    :--primary-color "#b45309"
    :--bg-color "#f3f4f6"
    :--surface-color "#ffffff"
    :--text-color "#111827"
    :--border-color "#d1d5db")

  (:body
    :margin 0
    :font-family "Inter, sans-serif"
    :background-color "var(--bg-color)"
    :color "var(--text-color)")

  (".chat-shell"
    :display "flex"
    :flex-direction "column"
    :height "100vh")

  (".chat-messages"
    :flex 1
    :overflow-y "auto"
    :padding 16)

  (".chat-form"
    :display "flex"
    :gap 12
    :padding 16
    :background-color "var(--surface-color)"
    :border-top "1px solid var(--border-color)")

  (".chat-input"
    :flex 1
    :padding 12
    :border "1px solid var(--border-color)"
    :border-radius 8
    :font-size 16
    :background-color "#ffffff")

  (".chat-button"
    :padding "12px 16px"
    :border 0
    :border-radius 8
    :background-color "var(--primary-color)"
    :color "#ffffff"
    :font-weight 600
    :cursor "pointer")

  (".chat-messages"
    ("@media (max-width: 600px)"
      :padding 8)))

(in-file "src/app/Document.tsx")
(import styles :from "../style.css?url")
(defcomponent *Document ((children :type 'React.ReactNode))
	      (html :lang "en"
		    (head
		     (meta :char-set "utf-8")
		     (meta :name "viewport" :content "width=device-width, initial-scale=1")
		     (title "@redwoodjs/starter-minimal")
		     (link :rel "stylesheet" :href styles)
		     (link :rel "modulepreload" :href "/src/client.tsx"))
		    (body
		     (div :id "root" children)
		     (script (import "/src/client.tsx")))))

(in-file "src/app/pages/Chat/functions.ts")
            (import (env) :from "cloudflare:workers")

            (defaction send-message ((prompt :type 'string))
              (console.log "Running AI with Prompt:" prompt)
              (let* ((response (await (env-call :ai :run
						"@cf/meta/llama-4-scout-17b-16e-instruct"

						(create :prompt prompt :stream t))))))
                (return response))

            (in-file "src/app/pages/Chat/Chat.tsx")
            (import (send-message) :from "./functions")
            (import (use-state) :from "react")
            (import (consume-event-stream) :from "rwsdk/client")

            (defclient *Chat ()
              (let-state
                ((message set-message) "")
                ((reply set-reply) "")
                ((is-loading set-is-loading) :false)
                (let* ((on-submit (async (lambda ((e :type 'React.FormEvent<HTMLFormElement>))
                                          (chain e (.prevent-default))
                                          (set-is-loading t)
                                          (set-reply "")
                                          (chain (await (send-message message))
                                                 (.pipe-to (consume-event-stream
                                                            (create :on-chunk
                                                                    (lambda (event)
                                                                      (set-reply (lambda (prev)
                                                                                   (if (= (@ event data) "[DONE]")
                                                                                       (progn
                                                                                         (set-is-loading :false)
                                                                                         (return prev))
                                                                                       (return (+ prev (@ (json-parse (@ event data)) response)))))))))))))))
                  (return
                    (div :class-name "flex flex-col h-screen"
                      (div :class-name "flex-1 overflow-y-auto p-4 mx-auto min-w-6/12 max-w-6/12" reply)
                      (form :on-submit on-submit :class-name "bg-white flex rounded-lg p-4"
                        (input :type "text" :value message :placeholder "Type a message..."
                               :on-change (lambda (e) (set-message (@ e target value)))
                               :class-name "w-full rounded-lg border-2 border-gray-300 p-2 mr-2")
                        (button :type "submit" :disabled (or (= (@ message length) 0) is-loading)
                                :class-name "rounded bg-amber-800 text-white px-4 py-2"
                                (|?:| is-loading "Sending..." "Send"))))))))

            (defapp :wrapper *Document :routes ("/" *Chat))

