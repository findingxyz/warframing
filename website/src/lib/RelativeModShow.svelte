<script>
    export let modData;
    export let showFusing = false;

    let frames = {'Common': 'Bronze', 'Uncommon': 'Silver', 'Rare': 'Gold', 'Legendary': 'Legendary', 'Riven': 'Omega', 'Immortal': 'Immortal'};

    let cdn = 'https://raw.githubusercontent.com/WFCD/genesis-assets/master/';

    let rankCompleteLine = cdn + 'modFrames/RankCompleteLine.png';
    let rankSlotActive = cdn + 'modFrames/RankSlotActive.png';
    let rankSlotEmpty = cdn + 'modFrames/RankSlotEmpty.png';

    let icon = 'https://raw.githubusercontent.com/WFCD/warframe-items/master/data/img/' + modData.imageName;

    let background = cdn + 'modFrames/' + frames[modData.rarity] + 'Background.png';
    let cornerLights = cdn + 'modFrames/' + frames[modData.rarity] + 'CornerLights.png';
    let bottomFrame = cdn + 'modFrames/' + frames[modData.rarity] + 'FrameBottom.png';
    let topFrame = cdn + 'modFrames/' + frames[modData.rarity] + 'FrameTop.png';
    let lowerTab = cdn + 'modFrames/' + frames[modData.rarity] + 'LowerTab.png';
    let sideLight = cdn + 'modFrames/' + frames[modData.rarity] + 'SideLight.png';
    let topRightBacker;

    let polarity;
    if (modData.polarity !== 'universal') {
        polarity = cdn + 'img/polarities/' + modData.polarity + '.png';
        topRightBacker = cdn + 'modFrames/' + frames[modData.rarity] + 'TopRightBacker.png';
    }

    let bottomFrameOffset = 280;
    if (modData.rarity === "Legendary") {
        bottomFrameOffset = 270;
    }

    let header;
    //header = cdn + 'modFrames/UmbraHeader.png';

    let exilus = cdn + 'img/polarities/umbra.png';

    let isExilus = false;

    let modName = modData.modName;
    let fusionLimit = modData.fusionLimit;
    let baseCapacity = modData.baseDrain;
    let fused = fusionLimit;
    let compatName = modData.compatName;
    let levelStats = modData.levelStats;
    let description = modData.description;

    let dtt = {
        "DT_IMPACT": "impact",
        "DT_PUNCTURE": "puncture",
        "DT_SLASH": "slash",
        "DT_FREEZE": "cold",
        "DT_ELECTRICITY": "electricity",
        "DT_FIRE": "heat",
        "DT_POISON": "toxin",
        "DT_EXPLOSION": "blast",
        "DT_CORROSIVE": "corrosive",
        "DT_GAS": "gas",
        "DT_MAGNETIC": "magnetic",
        "DT_RADIATION": "radiation",
        "DT_VIRAL": "viral",
    };

    function fuseLeast() {
      fused = 0;
    }
    function fuseLess() {
      fused = Math.max(0, fused - 1);
    }
    function fuseMore() {
      fused = Math.min(fusionLimit, fused + 1);
    }
    function fuseMost() {
      fused = fusionLimit;
    }
    function angleBracket(match) {
        if (match === "<LINE_SEPARATOR>") return "<hr>";
        if (match.substring(1, 3) === "DT") {
            let damageType = match.substring(1, match.length - 1);
            let imgsrc = cdn + 'img/damage/' + dtt[damageType] + '.png';
            return '<img src="' + imgsrc + '" alt="" style="max-height: 1em; filter: invert(1); position: relative; top: 1.5px; margin-right: 1px;" />';
        }
        return "???";
    }
</script>

{#if modData}
    {#if showFusing}
        <div id="fusing">
            <button on:click={fuseLeast}>
            --
            </button>
            <button on:click={fuseLess}>
            -
            </button>
            <button on:click={fuseMore}>
            +
            </button>
            <button on:click={fuseMost}>
            ++
            </button>
        </div>
    {/if}

    <div class="modshow">
        <img src={icon} alt="background" class="modbit bgicon" />
        <div class="shadow"></div>
        <div class="modbit modtext" style="background-image: url({background});">
            <div style="height: 8px;"></div>
            <div><span class="modname">{modName}</span></div>
            <div class="modstat">
                {#if levelStats}
                    {#each levelStats.at(fused).stats as stat}
                        <span>{@html stat.replace(/<[A-Z_]*>/g, angleBracket)}</span><br>
                        <!--<span>{stat}</span><br>-->
                    {/each}
                {:else}
                    <span>{description}</span>
                {/if}
            </div>
            <div style="height: 46px;"></div>
        </div>
        <img src={bottomFrame} alt="background" class="modbit bottomFrame" style="top: {bottomFrameOffset}px;" />
        <img src={topRightBacker} alt="background" class="modbit topRightBacker" />
        <img src={polarity} alt="background" class="modbit polarity" />
        <div class="modbit capacity"><span>{baseCapacity > 0 ? baseCapacity + fused: baseCapacity - fused}</span></div>
        <img src={topFrame} alt="background" class="modbit topFrame" />
        <img src={lowerTab} alt="background" class="modbit lowerTab" />
        <div class="modbit compat"><p>{compatName}</p></div>
        {#if header}
            <img src={header} alt="background" class="modbit header" />
        {/if}
        {#if fused === fusionLimit}
            <img src={rankCompleteLine} alt="background" class="modbit rankCompleteLine" />
        {/if}
        <div class="modbit rankslotrows">
            {#each [...Array(fusionLimit).keys()] as _, i}
                {#if i >= fused}
                    <img src={rankSlotEmpty} alt="background" />
                {:else}
                    <img src={rankSlotActive} alt="background" />
                {/if}
            {/each}
        </div>
        <img src={cornerLights} alt="background" class="modbit cornerLightsR" />
        <img src={cornerLights} alt="background" class="modbit cornerLightsL flipped" />
        <img src={sideLight} alt="background" class="modbit sideLightR" />
        <img src={sideLight} alt="background" class="modbit sideLightL flipped" />
    </div>
{/if}

<style>
    .modshow {
        position: relative;
        width: 256px;
        height: 400px;
        background-repeat: no-repeat;
        font-family: sans-serif;
    }
    .modbit {
        position: absolute;
    }
    .flipped {
        -moz-transform: scaleX(-1);
        -webkit-transform: scaleX(-1);
        -o-transform: scaleX(-1);
        transform: scaleX(-1);
    }
    .bgicon {
        object-fit: none;
        object-position: center -100%;
        width: 240px;
        left: 8px;
        top: 6px;
    }
    /*
    .bottomFrame {
        top: 270px;
        top: 280px;
    }
    */
    .topRightBacker {
        top: 30px;
        left: 205px;
        opacity: 90%;
    }
    .topFrame {
        top: 4px;
    }
    .lowerTab {
        top: 325px;
        left: 23px;
    }
    .header {
        top: 0px;
        left: 64px;
    }
    .sideLightL{
        top: 58px;
        left: 3px;
    }
    .sideLightR {
        top: 58px;
        left: 237px;
    }
    .cornerLightsR {
        top: 312px;
        left: 198px;
    }
    .cornerLightsL {
        top: 312px;
        left: -5px;
    }
    .rankCompleteLine {
        top: 375px;
    }
    .shadow {
        top: 20px;
        left: 7px;
        display: block;
        position: relative;
        width: 242px;
        height: 242px;
        box-sizing: content-box;
        background-size: cover;
        background-position: center 25%;
    }
    .shadow:before {
        display: block;
        content: '';
        position: absolute;
        width: 100%;
        height: 100%;
        box-shadow: 0 0 5px 5px #222222 inset;
    }
    .polarity {
        top: 34px;
        left: 230px;
        filter: invert(1);
        width: 16px;
    }
    .capacity {
        top: 36px;
        left: 213px;
        right: 26px;
        color: white;
        font-size: 14px;
        text-align: right;
    }
    .compat {
        text-transform: uppercase;
        text-align: center;
        font-weight: bold;
        width: 256px;
        top: 318px;
        left: 0px;
        color: white;
        font-size: 14px;
    }
    .modtext {
        background-position: center;
        text-align: center;
        color: white;
        width: 256px;
        bottom: 36px;
        min-height: 128px;
    }
    .modtext:before {
        left: 8px;
        width: 240px;
        height: 100%;
        content: '';
        box-shadow: 0px -2px 3px #222222;
        box-shadow: 0px -5px 5px 0px #222222;

        display: block;
        position: absolute;
    }
    .modname {
        font-size: 20px;
    }
    .modstat {
        width: 216px;
        padding: 0px 20px 4px;
        font-size: 16px;
        font-family: sans-serif;

    }
    .rankslotrows {
        text-align: center;
        bottom: 11px;
        width: 256px;
    }
</style>
