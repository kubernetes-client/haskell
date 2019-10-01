module Kubernetes.Client.Auth.Internal.Types where

import Network.TLS as TLS
import Kubernetes.Client.KubeConfig
import Kubernetes.OpenAPI (KubernetesClientConfig)

type DetectAuth = AuthInfo
                  -> (TLS.ClientParams, KubernetesClientConfig)
                  -> Maybe (IO (TLS.ClientParams, KubernetesClientConfig))
