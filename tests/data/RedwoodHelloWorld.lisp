(target :redwood)

(in-file "src/style.css")
(defcss app-styles
  (:root
    :--bg-color "#f4eedf"
    :--text-color "#1a1a1a"
    :--accent-color "#f47238"
    :--font-heading "\"Playfair Display\", serif"
    :--font-body "\"Noto Sans\", sans-serif")

  (:body
    :margin 0
    :padding 0
    :background-color "var(--bg-color)"
    :color "var(--text-color)"
    :font-family "var(--font-body)")

  (".container"
    :max-width 960
    :margin "0 auto"
    :padding "5rem 2rem"
    :min-height "100vh")

  (".title"
    :margin 0
    :font-family "var(--font-heading)"
    :font-size "4rem"
    :line-height 1.1)

  (".subtitle"
    :font-size "1.25rem"
    :margin-top "1rem")

  (".link"
    :color "var(--accent-color)"
    :font-weight 700
    :text-decoration "none"))

(in-file "src/app/Document.tsx")
(import styles :from "../style.css?url")
(defcomponent *Document ((children :type 'React.ReactNode))
  (html :lang "en"
    (head
      (meta :char-set "utf-8")
      (meta :name "viewport" :content "width=device-width, initial-scale=1")
      (title "Hello Redwood")
      (link :rel "preconnect" :href "https://fonts.googleapis.com")
      (link :rel "preconnect" :href "https://fonts.gstatic.com" :cross-origin "")
      (link :rel "stylesheet"
            :href "https://fonts.googleapis.com/css2?family=Noto+Sans:wght@400;700&family=Playfair+Display:wght@700&display=swap")
      (link :rel "stylesheet" :href styles)
      (link :rel "modulepreload" :href "/src/client.tsx"))
    (body
      children
      (script (import "/src/client.tsx")))))

(in-file "src/app/pages/Home/Home.tsx")
(defcomponent *Home ()
  (div :class-name "container"
    (h1 :class-name "title" "Hello Redwood")
    (p :class-name "subtitle"
       "Your first RedwoodSDK app is running.")
    (p
      (a :class-name "link"
         :href "https://docs.rwsdk.com/"
         :target "_blank"
         :rel "noreferrer"
         "Read the docs"))))

;; Header component with menu
(defcomponent *Header ()
  (header
	(nav
	  (ul
		(li (a :href "/" "Home"))
		(li (a :href "/about" "About"))
		(li (a :href "/contact" "Contact"))))))

(defapp :wrapper *Document :routes ("/" *Home))
