module internal MultiSet
    
    type MultiSet<'a when 'a : comparison> = 
        | M of Map<'a, uint32>
    
    let empty = M Map.empty
    let isEmpty (M s) = s.IsEmpty
    let size (M s) = if isEmpty (M s) then 0u else s |> Seq.sumBy(fun i -> i.Value)
    let contains a (M s) = s.ContainsKey(a)
    let numItems a (M s) = if contains a (M s) then s.[a] else 0u
    let add a n (M s) = M (s.Add(a, n))
    let addSingle a (M s) = if contains a (M s) then add a (s.[a]+1u) (M s) else add a 1u (M s)
    let remove a n (M s) = if s.[a] > n then add a (s.[a]-n) (M s) else M (s.Remove a)
    let removeSingle a (M s) = if contains a (M s) then remove a 1u (M s) else M (s)
 
    let fold f acc (M s) = Map.fold f acc s
    let foldBack f (M s) acc = Map.foldBack f s acc
    let toList (M s) =
        Map.fold (fun acc k v ->
            let array = Array.create (int v) k 
            Array.toList array @ acc 
        ) [] s