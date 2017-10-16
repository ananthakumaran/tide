<template>
  <p>{{ greeting }} World, visitor {{ visitorId }}!</p>
</template>

<script lang="ts">
import {Person} from './test';

const randomNumber: number = Math.random();
const notANumber: number = 'this should show an error';

const person = new Person('name', 'should have been a boolean');

export default {
  data: {
    visitorId: randomNumber
  },
  props: {
    todo: {
      type: Object,
      required: true
    }
  }
}
</script>

<style scoped>
p {
  font-size: 2em;
  text-align: center;
}
</style>
