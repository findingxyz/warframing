<script>
</script>

<div>
    <form class="stuff" action="/search" method="GET">
        <label>search for some mods
            <input id="search" class="centered"
                name="q"
            />
        </label>
    </form>
    <p class="stuff">
        Untagged searches search mod names. Tags can be used to narrow results down. You can compose a search entirely out of tags.
    </p>
    <ul class="stuff">
        <li><code>t:</code> or <code>type:</code> to search for type. e.g. <code>t:warframe</code></li>
        <li><code>r:</code> or <code>rarity:</code> to search for rarity. e.g. <code>r:u</code>, <code>r:rare</code></li>
        <li><code>p:</code> or <code>polarity:</code> to search for polarity. e.g. <code>p:naramon</code>, <code>p:pen</code></li>
        <li><code>c:</code> or <code>capacity:</code> to search for (base) mod capacity. (base search moving into <code>b:</code> soon)</li>
        <li><code>b:</code> or <code>capacity:</code> to search for base mod capacity. (coming soon)</li>
        <li><code>m:</code> or <code>maxcapacity:</code> to search for max mod capacity. e.g. <code>m&gt;=16</code>, <code>m&lt;8</code></li>
        <li><code>f:</code> or <code>fits:</code> to search for what mod fits where. e.g. <code>f:excal</code></li>
        <li><code>d:</code> or <code>description:</code> to search for level text. e.g. <code>d:health</code>, <code>d:"detects and collects"</code>. Will be <code>s:</code> or <code>stat:</code> in the future.</li>
        <li><code>u:</code> to search the unique name of a mod.</li>
    </ul>
    <p class="stuff">
        Some examples: <a href="/search?q=f:aura p:naramon"><code>f:aura p:naramon</code></a> searches for aura mods with a naramon polarity.
        <a href="/search?q=d:&quot;tau resistance&quot;"><code>d:"tau resistance"</code></a> finds mods with "tau resistance" in their descriptions.
        <a href="/search?q=strike"><code>strike</code></a> finds mods with "strike" in their names,
        <a href="/search?q=strike f:melee"><code>strike f:melee</code></a> specifically melee mods.

    </p>
</div>

<style>
    .stuff {
        box-sizing: border-box;
        margin: 30px auto 0;
        max-width: 25em;
    }
    #search {
        width: 100%;
        box-sizing: border-box;
    }
    .centered {
        margin: 0 auto;
    }
    code {
        background-color: #181818;
    }
</style>