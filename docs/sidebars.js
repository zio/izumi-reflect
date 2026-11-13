const sidebars = {
  sidebar: [
    {
      type: "category",
      label: "Izumi Reflect",
      collapsed: false,
      link: { type: "doc", id: "index" },
      items: [
        "getting-started",
        {
          type: "category",
          label: "Reference",
          collapsed: false,
          items: [
            "reference/tag",
            "reference/light-type-tag",
            "reference/ltag",
            "reference/scala-2-vs-scala-3",
            "reference/debugging",
            "reference/typetag-comparison"
          ]
        },
        {
          type: "category",
          label: "How-to Guides",
          collapsed: false,
          items: [
            "guides/heterogeneous-map",
            "guides/zio-environment",
            "guides/migrate-from-typetag",
            "guides/debug-a-wrong-tag"
          ]
        }
      ]
    }
  ]
};

module.exports = sidebars;
