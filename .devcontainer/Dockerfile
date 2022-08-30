ARG VARIANT="bullseye"
FROM debian:${VARIANT}

# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="true"
# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
COPY library-scripts/*.sh library-scripts/*.env /tmp/library-scripts/
RUN bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true" \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

RUN apt-get update \
    && apt-get install -y --no-install-recommends apt-utils bash build-essential ca-certificates curl gcc git gnupg libffi-dev libffi7 libgmp-dev libgmp-dev libgmp10 libicu-dev libncurses-dev libncurses5 libnuma1 libnuma-dev libtinfo5 lsb-release make procps software-properties-common sudo wget xz-utils z3 zlib1g-dev \
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/* 

USER vscode
WORKDIR /home/${USERNAME}
ENV PATH="/home/${USERNAME}/.local/bin:/home/${USERNAME}/.cabal/bin:/home/${USERNAME}/.ghcup/bin:$PATH"
RUN echo "export PATH=$PATH" >> /home/$USERNAME/.profile

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=yes \
    BOOTSTRAP_HASKELL_NO_UPGRADE=yes \
    BOOTSTRAP_HASKELL_INSTALL_HLS=yes

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
