(import trytond.pool [Pool]
        trytond.modules.hyton.sugar [save]
        trytond.modules.hyton.utils [first]
        )

(defn create-model [model-name #* args #** kwargs]
  ((.get (Pool) model-name)
    #* args #** kwargs))

(defn create-save-model [model-name #* args #** kwargs]
  (save
    (create-model model-name #* args #** kwargs)))

(defn reload-model [model]
  ((.get (Pool) model.__name__) model.id))

(defn load-first-model [model-name domain]
  (first (.search (.get (Pool) model-name) domain :limit 1)))
