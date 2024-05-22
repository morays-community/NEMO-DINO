# Generic Experiment Name

## Context and Motivation

[A DIabatic NeverwOrld2](https://github.com/vopikamm/DINO) coupled with external Python models.

#### Variations
- GZ21 : DINO coupled with [Guillaumin and Zanna, 2021](https://doi.org/10.1029/2021MS002534) ML model 

## Requirements

### Compilation

- NEMO version : [v4.2.1](https://forge.nemo-ocean.eu/nemo/nemo/-/releases/4.2.1) patched with [morays](https://github.com/morays-community/morays-doc/tree/main/nemo_src) and local `CONFIG/my_src` sources.

- Code Compilation manager : none, use standard `makenemo` script


### Python

- Eophis version : [v0.9.2](https://github.com/alexis-barge/eophis/tree/v0.9.2)


### Run

- NEMO Production Manager : none, use submission script `job.ksh` in `RUN`


### Post-Process

- No Post-Process libraries

- Plotting : Python script in `POSTPROCESS`, with `plots.yml`

