# ---------------------------------------------------------
# TA3 Kubernetes Pod Stub File
# ---------------------------------------------------------
## Specifications
# * Assumes a web service over ports 80 (http) and 443 https
# * Assumes a listening port 5001 to listen to TA2 signals
# * In this speicification, the ta3-main container has the web ports
# but one can have a separate container handle the web ports
# that is not the TA3-main container
---
# Web Port Service (perhaps Nginx)
apiVersion: v1
kind: Service
metadata:
  name: {{ eval_id }}-ta3-web
  labels:
    evalId: {{ eval_id }}
    tier: ta3
spec:
  # NodePort type allows to expose service outside the cluster
  type: NodePort
  selector:
    evalId: {{ eval_id }}
    tier: ta3
  ports:
    - name: http
      port: 80
      targetPort: 80
    - name: https
      port: 443
      targetPort: 443
---
# TA3 Pod
apiVersion: v1
kind: Pod
metadata:
  name: {{ eval_id }}-ta3
  labels:
    evalId: {{ eval_id }}
    main: "yes"
    tier: ta3
    ta3-main: "yes"
spec:
  restartPolicy: Never
  volumes:
    - name: "ravens-volume"
      hostPath:
        path: "/ravens_volume"
    ##====|Begin| Required Code that NIST uses to pair TA2 and TA3 components |Begin|
    - name: nfs-data
      persistentVolumeClaim:
        claimName: pvc-datasets
    - name: nfs-output
      persistentVolumeClaim:
        claimName: pvc-rw
    ##====|End| Required Code that NIST uses to pair TA2 and TA3 components |End|
  # affinity:
  #   # Link pod affinity with
  #   podAffinity:
  #     requiredDuringSchedulingIgnoredDuringExecution:
  #     - labelSelector:
  #         matchExpressions:
  #         - key: evalId
  #           operator: In
  #           values:
  #           - {{ eval_id }}
  #       topologyKey: failure-domain.beta.kubernetes.io/zone
  # -------------------------
  # Start container specs
  # -------------------------
  containers:
  # -------------------------
  # 1 of 3: Ravens nginx
  # - reverse proxy for ta3-main
  # -------------------------
  - name: ravens-nginx
    image: registry.datadrivendiscovery.org/j18_ta3eval/tworavens/ravens-nginx:stable
    imagePullPolicy: Always
    ports:
      - containerPort: 80
        name: http
        protocol: TCP
  # -------------------------
  # 2 of 3: TA3 main container
  # - includes entry point for evaluation
  # -------------------------
  - name: ta3-main
    image: registry.datadrivendiscovery.org/j18_ta3eval/tworavens/ravens-main:stable
    imagePullPolicy: Always
    command:
      - {{ command }}
    args:
      - {{ command_args }}
    # web (perhaps nginx) ports
    ports:
      # Main web port; nginx makes this available via port 80
      - containerPort: 8080
        name: http
        protocol: TCP
      #- containerPort: 443
      #  name: https
      #  protocol: TCP
      # Additional Port for TA3 to listen to TA2 signals
      #- containerPort: 5001
      #  protocol: TCP
    env:
      # environment variables passed to container
      - name: EVAL_ID
        value: {{ eval_id }}
      # --------------------------------------
      # Query TA2 Service with service name using format from template:
      # --------------------------------------
      - name: TA2_TEST_SERVER_URL
        value: {{ eval_id }}-ta2:45042
      # --------------------------------------
      # Test mode: returns "canned responses" instead
      # of actual TA2 connection
      # --------------------------------------
      - name: TA2_STATIC_TEST_MODE
        value: "False"
      - name: R_DEV_SERVER_BASE
        value: "http://localhost:8000/custom/"
      - name: TA2_PORT
        value: "45042"
      - name: TA3_PORT
        value: "5001"
    volumeMounts:
      - name: "ravens-volume"
        mountPath: "/ravens_volume"
        readOnly: false
      ##====|Begin| Required Code that NIST uses to pair TA2 and TA3 components |Begin|
      - name: nfs-data
        mountPath: "/inputs"
        subPath: "{{ path_to_dataroot }}"
        readOnly: true
      - name: "nfs-output"
        mountPath: "/outputs"
        subPath: "{{ path_to_outputs }}"
      ##====|End| Required Code that NIST uses to pair TA2 and TA3
    # liveness is for restart
    livenessProbe:
      exec:
        command:
        - cat
        - /var/webapps/TwoRavens/docs/health.md
      initialDelaySeconds: 60
      periodSeconds: 5
    readinessProbe:
      exec:
        command:
        - cat
        - /var/webapps/TwoRavens/docs/health.md
      initialDelaySeconds: 5
      periodSeconds: 5
  # -------------------------
  # 3 of 3: Rook R services
  # -------------------------
  - name: rook-service
    image: registry.datadrivendiscovery.org/j18_ta3eval/tworavens/ravens-r-service:stable
    imagePullPolicy: Always
    # same mounts as ta3-main and ta2-main
    volumeMounts:
      - name: "ravens-volume"
        mountPath: "/ravens_volume"
        readOnly: false
      ##====|Begin| Required Code that NIST uses to pair TA2 and TA3 components |Begin|
      - name: nfs-data
        mountPath: "/inputs"
        subPath: "{{ path_to_dataroot }}"
        readOnly: true
      - name: "nfs-output"
        mountPath: "/outputs"
        subPath: "{{ path_to_outputs }}"
      ##====|End| Required Code that NIST uses to pair TA2 and TA3
    # web port
    ports:
      - containerPort: 8000
        name: http
        protocol: TCP
    # environment variables passed to container
    env:
      - name: EVAL_ID
        value: {{ eval_id }}
      # Also set to "yes" in the Dockefile
      - name: ROOK_USE_PRODUCTION_MODE
        value: "yes"