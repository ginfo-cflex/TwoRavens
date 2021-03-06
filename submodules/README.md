# Submodules

This directory is used for incorporating other git repositories.

These repos are incorporated in two ways:

1. **git submodules**: e.g. pointers to other github repositories.
  - ref: https://git-scm.com/book/en/v2/Git-Tools-Submodules
1. copying in the repository manually
  - This is in the case for a required repository that is behind a password--though not private in a licensing sense

## Submodule Log

**3/12/2020**
- Update repository: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api/-/tags)
  - **version**:v2020.2.11
  - **method of inclusion**
    - downloaded tagged version manually as zip--it's in a private gitlab
    - unzip it and rename folder to ta3ta2-api
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in


**2/20/2020**
- Revert back to the v2019.12.4 version for now
  - This is for GCE deployments until TA2s are fully updated

**2/6/2020**
- Update repository: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api/-/tags)
  - **version**: v2020.1.28
  - **method of inclusion**
    - downloaded tagged version manually as zip--it's in a private gitlab
    - unzip it and rename folder to ta3ta2-api
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in

**1/13/2020**
- Update repository: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api/-/tags)
  - **version**: v2019.12.4
  - **method of inclusion**
    - downloaded tagged version manually as zip--it's in a private gitlab
    - rename to ta3ta2-api
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in

**7/10/2019**
- Update repository: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2019.7.9
  - **method of inclusion**
    - downloaded tagged version manually as zip--it's in a private gitlab
    - rename to ta3ta2-api
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in

**6/17/2019**
- Update repostiory: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2019.6.11
  - **method of inclusion**
    - downloaded tagged version manually as zip--it's in a private gitlab
    - rename to ta3ta2-api
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in


**5/21/2019**
- Update repostiory: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2019.4.11
  - **method of inclusion**
    - downloaded tagged version manually as zip--it's in a private gitlab
    - rename to ta3ta2-api
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in

**3/7/2019**
- Update repostiory: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2019.2.27
  - **method of inclusion**
    - downloaded version manually--it's in a private gitlab
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in

**2/2/2019**
- Update repostiory: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2019.1.22
  - **method of inclusion**
    - downloaded version manually--it's in a private gitlab
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in

**7/9/2018**

- Update repostiory: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2018.7.7
  - **method of inclusion**
    - downloaded version manually--it's in a private gitlab
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in


**6/26/2018**

- Remove repository [raven-metadata-service](https://github.com/TwoRavens/raven-metadata-service)
  - remove it for now; may be back soon


**6/11/2018**

- Add repository [raven-metadata-service](https://github.com/TwoRavens/raven-metadata-service)
  - **version**: master
  - **method of inclusion**
    - `git submodule add https://github.com/TwoRavens/raven-metadata-service.git`
    -
- Add repository: [ta3ta2-api](https://gitlab.com/datadrivendiscovery/ta3ta2-api)
  - **version**: v2018.6.2
  - **method of inclusion**
    - downloaded version manually--it's in a private gitlab
    - adding it to the `sys.path` in `settings/base.py`
    - ran `fab compile_ta3ta2_api`
    - checked it in
