<div id="content">
    <a href="/">home</a>

    <slot></slot>
</div>

<style>
    @import url('https://fonts.googleapis.com/css2?family=Lato:ital,wght@0,100;0,300;0,400;0,700;0,900;1,100;1,300;1,400;1,700;1,900&display=swap');
    :global(body) {
        margin: 0;
        font-family: Lato, sans-serif;
    }
    #content {
        /* background-image: linear-gradient(#1c0e49, #7e1913); */
        background-color: #1c0e49;
        color: #fff;
        min-height: 100vh;
    }
    :global(a) {
        color: #fff;
    }
    :global(a:visited) {
        color: #fff;
    }
</style>