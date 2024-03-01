<script>
    import { onMount } from 'svelte';
    import { page } from '$app/stores';

    import RelativeModShow from '../../lib/RelativeModShow.svelte'

    let mods;

    onMount(async () => {
        const query = $page.url.searchParams.get('q');
        const res = await fetch('/api/mods/search?q=' + encodeURIComponent(query))
        mods = await res.json();
    });
</script>

<div class="centered">
    <form data-sveltekit-reload action="/search" method="GET" class="centered">
        <label class="centered">
            search for a mod:
            <input
                name="q"
            />
        </label>
    </form>
</div>

<div>
    {#if mods}
        <h1 id="resultcount">{mods.length} results</h1>
        <div id="result" class="centered">
            {#each mods as mod}
                <div style="padding: 1em;">
                    <RelativeModShow modData={mod} />
                </div>

                <!--
                <div class="card centered">
                    <h2>{mod.modName}</h2>
                    <h4>{mod.baseDrain > -1 ? mod.baseDrain + mod.fusionLimit : mod.baseDrain - mod.fusionLimit} - {mod.polarity}</h4>
                    <h4>{mod.compatName}</h4>
                    {#if mod.levelStats}
                        {#each mod.levelStats.at(-1).stats as stat}
                            <p>{stat}</p>
                        {/each}
                    {/if}
                    {#if mod.description}
                        <p>{mod.description}</p>
                    {/if}
                </div>
                -->
            {/each}
        </div>
    {:else}
        <p>loading...</p>
    {/if}
</div>

<style>
    #result {
        display: flex;
        flex-flow: row wrap;
        font-size: 14px;
        width: 80%;
    }
    #resultcount {
        margin: 0 1em;
    }
    /*
    .card {
        position: relative;
        display: block;
        margin: 0 0 9px 0;
        width: 80%;
    }
    @media only screen and (min-width: 500px) {
        .card {
            width: 49.25%;
        }
    }
    @media only screen and (min-width: 700px) {
        .card {
            width: 32.75%;
        }
    }
    @media only screen and (min-width: 1000px) {
        .card {
            width: 24.5%;
        }
    }
    */
    .centered {
        margin: 0 auto;
    }
</style>
