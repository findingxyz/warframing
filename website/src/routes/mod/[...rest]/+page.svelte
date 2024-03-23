<script>
    import { onMount } from 'svelte';

    import RelativeModShow from '../../../lib/RelativeModShow.svelte'

	export let data;

	let mod;
   
	const query = '/' + data.post;

	onMount(async () => {
        const res = await fetch('/api/mod?q=' + encodeURIComponent(query));
        mod = await res.json();
    });
</script>

{#if mod}
	<RelativeModShow showFusing=true modData={mod} />
{/if}
