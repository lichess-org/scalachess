package chess

import chess.format.Uci

final class Hash(size: Int) {

  private def roleIndex(role: Role) = role match {
    case Pawn   => 0
    case Knight => 1
    case Bishop => 2
    case Rook   => 3
    case Queen  => 4
    case King   => 5
  }

  private def pieceIndex(piece: Piece) =
    roleIndex(piece.role) * 2 + piece.color.fold(1, 0)

  private def posIndex(pos: Pos) =
    8 * pos.y + (pos.x - 9)

  private def actorIndex(actor: Actor) =
    64 * pieceIndex(actor.piece) + posIndex(actor.pos)

  private def crazyPocketMask(role: Role, colorshift: Int, count: Int) = {
    // There should be no kings and at most 16 pieces of any given type
    // in a pocket.
    if (0 < count && count <= 16 && roleIndex(role) < 5) Some {
      Hash.crazyPocketMasks(16 * roleIndex(role) + count + colorshift)
    }
    else None
  }

  private[chess] def hexToBytes(str: String): PositionHash = {
    str.grouped(2).map(cc =>
      (Character.digit(cc(0), 16) << 4 | Character.digit(cc(1), 16)).toByte
    ).take(size).toArray
  }

  def apply(situation: Situation): PositionHash = {

    val board = situation.board
    val hturn = situation.color.fold(Hash.whiteTurnMask, 0l)

    val hactors = board.actors.values.view.map {
      Hash.actorMasks compose actorIndex _
    }.fold(hturn)(_ ^ _)

    val hcastling =
      if (board.variant.allowsCastling)
        (situation.history.castles.toList zip Hash.castlingMasks).collect {
          case (true, castlingMask) => castlingMask
        }.fold(hactors)(_ ^ _)
      else hactors

    val hep = situation.enPassantSquare match {
      case Some(pos) => hcastling ^ Hash.enPassantMasks(pos.x - 1)
      case None      => hcastling
    }

    // Hash in special three-check data.
    val hchecks = board.variant match {
      case variant.ThreeCheck =>
        val blackCount = math.min(situation.history.checkCount.black, 3)
        val whiteCount = math.min(situation.history.checkCount.white, 3)
        val hblackchecks = if (blackCount > 0) hep ^ Hash.threeCheckMasks(blackCount - 1) else hep
        if (whiteCount > 0) hblackchecks ^ Hash.threeCheckMasks(whiteCount + 2) else hblackchecks
      case _ => hep
    }

    // Hash in special crazyhouse data.
    val hcrazy = board.crazyData match {
      case Some(data) =>
        val hcrazypromotions = data.promoted.toList.map { Hash.crazyPromotionMasks compose posIndex _ }.fold(hchecks)(_ ^ _)
        Color.all.flatMap { color =>
        val colorshift = color.fold(79, -1)
        data.pockets(color).roles.groupBy(identity).flatMap {
          case (role, list) => crazyPocketMask(role, colorshift, list.size)
        }
      }.fold(hcrazypromotions)(_ ^ _)
      case None => hchecks
    }

    Array.tabulate(size)(i => (hcrazy >>> ((7 - i) * 8)).toByte)
  }

}

object Hash {

  val size = 3

  // The following masks are compatible with the Polyglot
  // opening book format.

  private val whiteTurnMask = 0xf8d626aaaf278509l

  private val actorMasks = Array(
    0x9d39247e33776d41l, 0x2af7398005aaa5c7l,
    0x44db015024623547l, 0x9c15f73e62a76ae2l,
    0x75834465489c0c89l, 0x3290ac3a203001bfl,
    0x0fbbad1f61042279l, 0xe83a908ff2fb60cal,
    0x0d7e765d58755c10l, 0x1a083822ceafe02dl,
    0x9605d5f0e25ec3b0l, 0xd021ff5cd13a2ed5l,
    0x40bdf15d4a672e32l, 0x011355146fd56395l,
    0x5db4832046f3d9e5l, 0x239f8b2d7ff719ccl,
    0x05d1a1ae85b49aa1l, 0x679f848f6e8fc971l,
    0x7449bbff801fed0bl, 0x7d11cdb1c3b7adf0l,
    0x82c7709e781eb7ccl, 0xf3218f1c9510786cl,
    0x331478f3af51bbe6l, 0x4bb38de5e7219443l,
    0xaa649c6ebcfd50fcl, 0x8dbd98a352afd40bl,
    0x87d2074b81d79217l, 0x19f3c751d3e92ae1l,
    0xb4ab30f062b19abfl, 0x7b0500ac42047ac4l,
    0xc9452ca81a09d85dl, 0x24aa6c514da27500l,
    0x4c9f34427501b447l, 0x14a68fd73c910841l,
    0xa71b9b83461cbd93l, 0x03488b95b0f1850fl,
    0x637b2b34ff93c040l, 0x09d1bc9a3dd90a94l,
    0x3575668334a1dd3bl, 0x735e2b97a4c45a23l,
    0x18727070f1bd400bl, 0x1fcbacd259bf02e7l,
    0xd310a7c2ce9b6555l, 0xbf983fe0fe5d8244l,
    0x9f74d14f7454a824l, 0x51ebdc4ab9ba3035l,
    0x5c82c505db9ab0fal, 0xfcf7fe8a3430b241l,
    0x3253a729b9ba3ddel, 0x8c74c368081b3075l,
    0xb9bc6c87167c33e7l, 0x7ef48f2b83024e20l,
    0x11d505d4c351bd7fl, 0x6568fca92c76a243l,
    0x4de0b0f40f32a7b8l, 0x96d693460cc37e5dl,
    0x42e240cb63689f2fl, 0x6d2bdcdae2919661l,
    0x42880b0236e4d951l, 0x5f0f4a5898171bb6l,
    0x39f890f579f92f88l, 0x93c5b5f47356388bl,
    0x63dc359d8d231b78l, 0xec16ca8aea98ad76l,
    0x5355f900c2a82dc7l, 0x07fb9f855a997142l,
    0x5093417aa8a7ed5el, 0x7bcbc38da25a7f3cl,
    0x19fc8a768cf4b6d4l, 0x637a7780decfc0d9l,
    0x8249a47aee0e41f7l, 0x79ad695501e7d1e8l,
    0x14acbaf4777d5776l, 0xf145b6beccdea195l,
    0xdabf2ac8201752fcl, 0x24c3c94df9c8d3f6l,
    0xbb6e2924f03912eal, 0x0ce26c0b95c980d9l,
    0xa49cd132bfbf7cc4l, 0xe99d662af4243939l,
    0x27e6ad7891165c3fl, 0x8535f040b9744ff1l,
    0x54b3f4fa5f40d873l, 0x72b12c32127fed2bl,
    0xee954d3c7b411f47l, 0x9a85ac909a24eaa1l,
    0x70ac4cd9f04f21f5l, 0xf9b89d3e99a075c2l,
    0x87b3e2b2b5c907b1l, 0xa366e5b8c54f48b8l,
    0xae4a9346cc3f7cf2l, 0x1920c04d47267bbdl,
    0x87bf02c6b49e2ae9l, 0x092237ac237f3859l,
    0xff07f64ef8ed14d0l, 0x8de8dca9f03cc54el,
    0x9c1633264db49c89l, 0xb3f22c3d0b0b38edl,
    0x390e5fb44d01144bl, 0x5bfea5b4712768e9l,
    0x1e1032911fa78984l, 0x9a74acb964e78cb3l,
    0x4f80f7a035dafb04l, 0x6304d09a0b3738c4l,
    0x2171e64683023a08l, 0x5b9b63eb9ceff80cl,
    0x506aacf489889342l, 0x1881afc9a3a701d6l,
    0x6503080440750644l, 0xdfd395339cdbf4a7l,
    0xef927dbcf00c20f2l, 0x7b32f7d1e03680ecl,
    0xb9fd7620e7316243l, 0x05a7e8a57db91b77l,
    0xb5889c6e15630a75l, 0x4a750a09ce9573f7l,
    0xcf464cec899a2f8al, 0xf538639ce705b824l,
    0x3c79a0ff5580ef7fl, 0xede6c87f8477609dl,
    0x799e81f05bc93f31l, 0x86536b8cf3428a8cl,
    0x97d7374c60087b73l, 0xa246637cff328532l,
    0x043fcae60cc0eba0l, 0x920e449535dd359el,
    0x70eb093b15b290ccl, 0x73a1921916591cbdl,
    0x56436c9fe1a1aa8dl, 0xefac4b70633b8f81l,
    0xbb215798d45df7afl, 0x45f20042f24f1768l,
    0x930f80f4e8eb7462l, 0xff6712ffcfd75ea1l,
    0xae623fd67468aa70l, 0xdd2c5bc84bc8d8fcl,
    0x7eed120d54cf2dd9l, 0x22fe545401165f1cl,
    0xc91800e98fb99929l, 0x808bd68e6ac10365l,
    0xdec468145b7605f6l, 0x1bede3a3aef53302l,
    0x43539603d6c55602l, 0xaa969b5c691ccb7al,
    0xa87832d392efee56l, 0x65942c7b3c7e11ael,
    0xded2d633cad004f6l, 0x21f08570f420e565l,
    0xb415938d7da94e3cl, 0x91b859e59ecb6350l,
    0x10cff333e0ed804al, 0x28aed140be0bb7ddl,
    0xc5cc1d89724fa456l, 0x5648f680f11a2741l,
    0x2d255069f0b7dab3l, 0x9bc5a38ef729abd4l,
    0xef2f054308f6a2bcl, 0xaf2042f5cc5c2858l,
    0x480412bab7f5be2al, 0xaef3af4a563dfe43l,
    0x19afe59ae451497fl, 0x52593803dff1e840l,
    0xf4f076e65f2ce6f0l, 0x11379625747d5af3l,
    0xbce5d2248682c115l, 0x9da4243de836994fl,
    0x066f70b33fe09017l, 0x4dc4de189b671a1cl,
    0x51039ab7712457c3l, 0xc07a3f80c31fb4b4l,
    0xb46ee9c5e64a6e7cl, 0xb3819a42abe61c87l,
    0x21a007933a522a20l, 0x2df16f761598aa4fl,
    0x763c4a1371b368fdl, 0xf793c46702e086a0l,
    0xd7288e012aeb8d31l, 0xde336a2a4bc1c44bl,
    0x0bf692b38d079f23l, 0x2c604a7a177326b3l,
    0x4850e73e03eb6064l, 0xcfc447f1e53c8e1bl,
    0xb05ca3f564268d99l, 0x9ae182c8bc9474e8l,
    0xa4fc4bd4fc5558cal, 0xe755178d58fc4e76l,
    0x69b97db1a4c03dfel, 0xf9b5b7c4acc67c96l,
    0xfc6a82d64b8655fbl, 0x9c684cb6c4d24417l,
    0x8ec97d2917456ed0l, 0x6703df9d2924e97el,
    0xc547f57e42a7444el, 0x78e37644e7cad29el,
    0xfe9a44e9362f05fal, 0x08bd35cc38336615l,
    0x9315e5eb3a129acel, 0x94061b871e04df75l,
    0xdf1d9f9d784ba010l, 0x3bba57b68871b59dl,
    0xd2b7adeeded1f73fl, 0xf7a255d83bc373f8l,
    0xd7f4f2448c0ceb81l, 0xd95be88cd210ffa7l,
    0x336f52f8ff4728e7l, 0xa74049dac312ac71l,
    0xa2f61bb6e437fdb5l, 0x4f2a5cb07f6a35b3l,
    0x87d380bda5bf7859l, 0x16b9f7e06c453a21l,
    0x7ba2484c8a0fd54el, 0xf3a678cad9a2e38cl,
    0x39b0bf7dde437ba2l, 0xfcaf55c1bf8a4424l,
    0x18fcf680573fa594l, 0x4c0563b89f495ac3l,
    0x40e087931a00930dl, 0x8cffa9412eb642c1l,
    0x68ca39053261169fl, 0x7a1ee967d27579e2l,
    0x9d1d60e5076f5b6fl, 0x3810e399b6f65ba2l,
    0x32095b6d4ab5f9b1l, 0x35cab62109dd038al,
    0xa90b24499fcfafb1l, 0x77a225a07cc2c6bdl,
    0x513e5e634c70e331l, 0x4361c0ca3f692f12l,
    0xd941aca44b20a45bl, 0x528f7c8602c5807bl,
    0x52ab92beb9613989l, 0x9d1dfa2efc557f73l,
    0x722ff175f572c348l, 0x1d1260a51107fe97l,
    0x7a249a57ec0c9ba2l, 0x04208fe9e8f7f2d6l,
    0x5a110c6058b920a0l, 0x0cd9a497658a5698l,
    0x56fd23c8f9715a4cl, 0x284c847b9d887aael,
    0x04feabfbbdb619cbl, 0x742e1e651c60ba83l,
    0x9a9632e65904ad3cl, 0x881b82a13b51b9e2l,
    0x506e6744cd974924l, 0xb0183db56ffc6a79l,
    0x0ed9b915c66ed37el, 0x5e11e86d5873d484l,
    0xf678647e3519ac6el, 0x1b85d488d0f20cc5l,
    0xdab9fe6525d89021l, 0x0d151d86adb73615l,
    0xa865a54edcc0f019l, 0x93c42566aef98ffbl,
    0x99e7afeabe000731l, 0x48cbff086ddf285al,
    0x7f9b6af1ebf78bafl, 0x58627e1a149bba21l,
    0x2cd16e2abd791e33l, 0xd363eff5f0977996l,
    0x0ce2a38c344a6eedl, 0x1a804aadb9cfa741l,
    0x907f30421d78c5del, 0x501f65edb3034d07l,
    0x37624ae5a48fa6e9l, 0x957baf61700cff4el,
    0x3a6c27934e31188al, 0xd49503536abca345l,
    0x088e049589c432e0l, 0xf943aee7febf21b8l,
    0x6c3b8e3e336139d3l, 0x364f6ffa464ee52el,
    0xd60f6dcedc314222l, 0x56963b0dca418fc0l,
    0x16f50edf91e513afl, 0xef1955914b609f93l,
    0x565601c0364e3228l, 0xecb53939887e8175l,
    0xbac7a9a18531294bl, 0xb344c470397bba52l,
    0x65d34954daf3cebdl, 0xb4b81b3fa97511e2l,
    0xb422061193d6f6a7l, 0x071582401c38434dl,
    0x7a13f18bbedc4ff5l, 0xbc4097b116c524d2l,
    0x59b97885e2f2ea28l, 0x99170a5dc3115544l,
    0x6f423357e7c6a9f9l, 0x325928ee6e6f8794l,
    0xd0e4366228b03343l, 0x565c31f7de89ea27l,
    0x30f5611484119414l, 0xd873db391292ed4fl,
    0x7bd94e1d8e17debcl, 0xc7d9f16864a76e94l,
    0x947ae053ee56e63cl, 0xc8c93882f9475f5fl,
    0x3a9bf55ba91f81cal, 0xd9a11fbb3d9808e4l,
    0x0fd22063edc29fcal, 0xb3f256d8aca0b0b9l,
    0xb03031a8b4516e84l, 0x35dd37d5871448afl,
    0xe9f6082b05542e4el, 0xebfafa33d7254b59l,
    0x9255abb50d532280l, 0xb9ab4ce57f2d34f3l,
    0x693501d628297551l, 0xc62c58f97dd949bfl,
    0xcd454f8f19c5126al, 0xbbe83f4ecc2bdecbl,
    0xdc842b7e2819e230l, 0xba89142e007503b8l,
    0xa3bc941d0a5061cbl, 0xe9f6760e32cd8021l,
    0x09c7e552bc76492fl, 0x852f54934da55cc9l,
    0x8107fccf064fcf56l, 0x098954d51fff6580l,
    0x23b70edb1955c4bfl, 0xc330de426430f69dl,
    0x4715ed43e8a45c0al, 0xa8d7e4dab780a08dl,
    0x0572b974f03ce0bbl, 0xb57d2e985e1419c7l,
    0xe8d9ecbe2cf3d73fl, 0x2fe4b17170e59750l,
    0x11317ba87905e790l, 0x7fbf21ec8a1f45ecl,
    0x1725cabfcb045b00l, 0x964e915cd5e2b207l,
    0x3e2b8bcbf016d66dl, 0xbe7444e39328a0acl,
    0xf85b2b4fbcde44b7l, 0x49353fea39ba63b1l,
    0x1dd01aafcd53486al, 0x1fca8a92fd719f85l,
    0xfc7c95d827357afal, 0x18a6a990c8b35ebdl,
    0xcccb7005c6b9c28dl, 0x3bdbb92c43b17f26l,
    0xaa70b5b4f89695a2l, 0xe94c39a54a98307fl,
    0xb7a0b174cff6f36el, 0xd4dba84729af48adl,
    0x2e18bc1ad9704a68l, 0x2de0966daf2f8b1cl,
    0xb9c11d5b1e43a07el, 0x64972d68dee33360l,
    0x94628d38d0c20584l, 0xdbc0d2b6ab90a559l,
    0xd2733c4335c6a72fl, 0x7e75d99d94a70f4dl,
    0x6ced1983376fa72bl, 0x97fcaacbf030bc24l,
    0x7b77497b32503b12l, 0x8547eddfb81ccb94l,
    0x79999cdff70902cbl, 0xcffe1939438e9b24l,
    0x829626e3892d95d7l, 0x92fae24291f2b3f1l,
    0x63e22c147b9c3403l, 0xc678b6d860284a1cl,
    0x5873888850659ae7l, 0x0981dcd296a8736dl,
    0x9f65789a6509a440l, 0x9ff38fed72e9052fl,
    0xe479ee5b9930578cl, 0xe7f28ecd2d49eecdl,
    0x56c074a581ea17fel, 0x5544f7d774b14aefl,
    0x7b3f0195fc6f290fl, 0x12153635b2c0cf57l,
    0x7f5126dbba5e0ca7l, 0x7a76956c3eafb413l,
    0x3d5774a11d31ab39l, 0x8a1b083821f40cb4l,
    0x7b4a38e32537df62l, 0x950113646d1d6e03l,
    0x4da8979a0041e8a9l, 0x3bc36e078f7515d7l,
    0x5d0a12f27ad310d1l, 0x7f9d1a2e1ebe1327l,
    0xda3a361b1c5157b1l, 0xdcdd7d20903d0c25l,
    0x36833336d068f707l, 0xce68341f79893389l,
    0xab9090168dd05f34l, 0x43954b3252dc25e5l,
    0xb438c2b67f98e5e9l, 0x10dcd78e3851a492l,
    0xdbc27ab5447822bfl, 0x9b3cdb65f82ca382l,
    0xb67b7896167b4c84l, 0xbfced1b0048eac50l,
    0xa9119b60369ffebdl, 0x1fff7ac80904bf45l,
    0xac12fb171817eee7l, 0xaf08da9177dda93dl,
    0x1b0cab936e65c744l, 0xb559eb1d04e5e932l,
    0xc37b45b3f8d6f2bal, 0xc3a9dc228caac9e9l,
    0xf3b8b6675a6507ffl, 0x9fc477de4ed681dal,
    0x67378d8eccef96cbl, 0x6dd856d94d259236l,
    0xa319ce15b0b4db31l, 0x073973751f12dd5el,
    0x8a8e849eb32781a5l, 0xe1925c71285279f5l,
    0x74c04bf1790c0efel, 0x4dda48153c94938al,
    0x9d266d6a1cc0542cl, 0x7440fb816508c4fel,
    0x13328503df48229fl, 0xd6bf7baee43cac40l,
    0x4838d65f6ef6748fl, 0x1e152328f3318deal,
    0x8f8419a348f296bfl, 0x72c8834a5957b511l,
    0xd7a023a73260b45cl, 0x94ebc8abcfb56dael,
    0x9fc10d0f989993e0l, 0xde68a2355b93cae6l,
    0xa44cfe79ae538bbel, 0x9d1d84fcce371425l,
    0x51d2b1ab2ddfb636l, 0x2fd7e4b9e72cd38cl,
    0x65ca5b96b7552210l, 0xdd69a0d8ab3b546dl,
    0x604d51b25fbf70e2l, 0x73aa8a564fb7ac9el,
    0x1a8c1e992b941148l, 0xaac40a2703d9bea0l,
    0x764dbeae7fa4f3a6l, 0x1e99b96e70a9be8bl,
    0x2c5e9deb57ef4743l, 0x3a938fee32d29981l,
    0x26e6db8ffdf5adfel, 0x469356c504ec9f9dl,
    0xc8763c5b08d1908cl, 0x3f6c6af859d80055l,
    0x7f7cc39420a3a545l, 0x9bfb227ebdf4c5cel,
    0x89039d79d6fc5c5cl, 0x8fe88b57305e2ab6l,
    0xa09e8c8c35ab96del, 0xfa7e393983325753l,
    0xd6b6d0ecc617c699l, 0xdfea21ea9e7557e3l,
    0xb67c1fa481680af8l, 0xca1e3785a9e724e5l,
    0x1cfc8bed0d681639l, 0xd18d8549d140caeal,
    0x4ed0fe7e9dc91335l, 0xe4dbf0634473f5d2l,
    0x1761f93a44d5aefel, 0x53898e4c3910da55l,
    0x734de8181f6ec39al, 0x2680b122baa28d97l,
    0x298af231c85bafabl, 0x7983eed3740847d5l,
    0x66c1a2a1a60cd889l, 0x9e17e49642a3e4c1l,
    0xedb454e7badc0805l, 0x50b704cab602c329l,
    0x4cc317fb9cddd023l, 0x66b4835d9eafea22l,
    0x219b97e26ffc81bdl, 0x261e4e4c0a333a9dl,
    0x1fe2cca76517db90l, 0xd7504dfa8816edbbl,
    0xb9571fa04dc089c8l, 0x1ddc0325259b27del,
    0xcf3f4688801eb9aal, 0xf4f5d05c10cab243l,
    0x38b6525c21a42b0el, 0x36f60e2ba4fa6800l,
    0xeb3593803173e0cel, 0x9c4cd6257c5a3603l,
    0xaf0c317d32adaa8al, 0x258e5a80c7204c4bl,
    0x8b889d624d44885dl, 0xf4d14597e660f855l,
    0xd4347f66ec8941c3l, 0xe699ed85b0dfb40dl,
    0x2472f6207c2d0484l, 0xc2a1e7b5b459aeb5l,
    0xab4f6451cc1d45ecl, 0x63767572ae3d6174l,
    0xa59e0bd101731a28l, 0x116d0016cb948f09l,
    0x2cf9c8ca052f6e9fl, 0x0b090a7560a968e3l,
    0xabeeddb2dde06ff1l, 0x58efc10b06a2068dl,
    0xc6e57a78fbd986e0l, 0x2eab8ca63ce802d7l,
    0x14a195640116f336l, 0x7c0828dd624ec390l,
    0xd74bbe77e6116ac7l, 0x804456af10f5fb53l,
    0xebe9ea2adf4321c7l, 0x03219a39ee587a30l,
    0x49787fef17af9924l, 0xa1e9300cd8520548l,
    0x5b45e522e4b1b4efl, 0xb49c3b3995091a36l,
    0xd4490ad526f14431l, 0x12a8f216af9418c2l,
    0x001f837cc7350524l, 0x1877b51e57a764d5l,
    0xa2853b80f17f58eel, 0x993e1de72d36d310l,
    0xb3598080ce64a656l, 0x252f59cf0d9f04bbl,
    0xd23c8e176d113600l, 0x1bda0492e7e4586el,
    0x21e0bd5026c619bfl, 0x3b097adaf088f94el,
    0x8d14dedb30be846el, 0xf95cffa23af5f6f4l,
    0x3871700761b3f743l, 0xca672b91e9e4fa16l,
    0x64c8e531bff53b55l, 0x241260ed4ad1e87dl,
    0x106c09b972d2e822l, 0x7fba195410e5ca30l,
    0x7884d9bc6cb569d8l, 0x0647dfedcd894a29l,
    0x63573ff03e224774l, 0x4fc8e9560f91b123l,
    0x1db956e450275779l, 0xb8d91274b9e9d4fbl,
    0xa2ebee47e2fbfce1l, 0xd9f1f30ccd97fb09l,
    0xefed53d75fd64e6bl, 0x2e6d02c36017f67fl,
    0xa9aa4d20db084e9bl, 0xb64be8d8b25396c1l,
    0x70cb6af7c2d5bcf0l, 0x98f076a4f7a2322el,
    0xbf84470805e69b5fl, 0x94c3251f06f90cf3l,
    0x3e003e616a6591e9l, 0xb925a6cd0421aff3l,
    0x61bdd1307c66e300l, 0xbf8d5108e27e0d48l,
    0x240ab57a8b888b20l, 0xfc87614baf287e07l,
    0xef02cdd06ffdb432l, 0xa1082c0466df6c0al,
    0x8215e577001332c8l, 0xd39bb9c3a48db6cfl,
    0x2738259634305c14l, 0x61cf4f94c97df93dl,
    0x1b6baca2ae4e125bl, 0x758f450c88572e0bl,
    0x959f587d507a8359l, 0xb063e962e045f54dl,
    0x60e8ed72c0dff5d1l, 0x7b64978555326f9fl,
    0xfd080d236da814bal, 0x8c90fd9b083f4558l,
    0x106f72fe81e2c590l, 0x7976033a39f7d952l,
    0xa4ec0132764ca04bl, 0x733ea705fae4fa77l,
    0xb4d8f77bc3e56167l, 0x9e21f4f903b33fd9l,
    0x9d765e419fb69f6dl, 0xd30c088ba61ea5efl,
    0x5d94337fbfaf7f5bl, 0x1a4e4822eb4d7a59l,
    0x6ffe73e81b637fb3l, 0xddf957bc36d8b9cal,
    0x64d0e29eea8838b3l, 0x08dd9bdfd96b9f63l,
    0x087e79e5a57d1d13l, 0xe328e230e3e2b3fbl,
    0x1c2559e30f0946bel, 0x720bf5f26f4d2eaal,
    0xb0774d261cc609dbl, 0x443f64ec5a371195l,
    0x4112cf68649a260el, 0xd813f2fab7f5c5cal,
    0x660d3257380841eel, 0x59ac2c7873f910a3l,
    0xe846963877671a17l, 0x93b633abfa3469f8l,
    0xc0c0f5a60ef4cdcfl, 0xcaf21ecd4377b28cl,
    0x57277707199b8175l, 0x506c11b9d90e8b1dl,
    0xd83cc2687a19255fl, 0x4a29c6465a314cd1l,
    0xed2df21216235097l, 0xb5635c95ff7296e2l,
    0x22af003ab672e811l, 0x52e762596bf68235l,
    0x9aeba33ac6ecc6b0l, 0x944f6de09134dfb6l,
    0x6c47bec883a7de39l, 0x6ad047c430a12104l,
    0xa5b1cfdba0ab4067l, 0x7c45d833aff07862l,
    0x5092ef950a16da0bl, 0x9338e69c052b8e7bl,
    0x455a4b4cfe30e3f5l, 0x6b02e63195ad0cf8l,
    0x6b17b224bad6bf27l, 0xd1e0ccd25bb9c169l,
    0xde0c89a556b9ae70l, 0x50065e535a213cf6l,
    0x9c1169fa2777b874l, 0x78edefd694af1eedl,
    0x6dc93d9526a50e68l, 0xee97f453f06791edl,
    0x32ab0edb696703d3l, 0x3a6853c7e70757a7l,
    0x31865ced6120f37dl, 0x67fef95d92607890l,
    0x1f2b1d1f15f6dc9cl, 0xb69e38a8965c6b65l,
    0xaa9119ff184cccf4l, 0xf43c732873f24c13l,
    0xfb4a3d794a9a80d2l, 0x3550c2321fd6109cl,
    0x371f77e76bb8417el, 0x6bfa9aae5ec05779l,
    0xcd04f3ff001a4778l, 0xe3273522064480cal,
    0x9f91508bffcfc14al, 0x049a7f41061a9e60l,
    0xfcb6be43a9f2fe9bl, 0x08de8a1c7797da9bl,
    0x8f9887e6078735a1l, 0xb5b4071dbfc73a66l,
    0x230e343dfba08d33l, 0x43ed7f5a0fae657dl,
    0x3a88a0fbbcb05c63l, 0x21874b8b4d2dbc4fl,
    0x1bdea12e35f6a8c9l, 0x53c065c6c8e63528l,
    0xe34a1d250e7a8d6bl, 0xd6b04d3b7651dd7el,
    0x5e90277e7cb39e2dl, 0x2c046f22062dc67dl,
    0xb10bb459132d0a26l, 0x3fa9ddfb67e2f199l,
    0x0e09b88e1914f7afl, 0x10e8b35af3eeab37l,
    0x9eedeca8e272b933l, 0xd4c718bc4ae8ae5fl,
    0x81536d601170fc20l, 0x91b534f885818a06l,
    0xec8177f83f900978l, 0x190e714fada5156el,
    0xb592bf39b0364963l, 0x89c350c893ae7dc1l,
    0xac042e70f8b383f2l, 0xb49b52e587a1ee60l,
    0xfb152fe3ff26da89l, 0x3e666e6f69ae2c15l,
    0x3b544ebe544c19f9l, 0xe805a1e290cf2456l,
    0x24b33c9d7ed25117l, 0xe74733427b72f0c1l,
    0x0a804d18b7097475l, 0x57e3306d881edb4fl,
    0x4ae7d6a36eb5dbcbl, 0x2d8d5432157064c8l,
    0xd1e649de1e7f268bl, 0x8a328a1cedfe552cl,
    0x07a3aec79624c7dal, 0x84547ddc3e203c94l,
    0x990a98fd5071d263l, 0x1a4ff12616eefc89l,
    0xf6f7fd1431714200l, 0x30c05b1ba332f41cl,
    0x8d2636b81555a786l, 0x46c9feb55d120902l,
    0xccec0a73b49c9921l, 0x4e9d2827355fc492l,
    0x19ebb029435dcb0fl, 0x4659d2b743848a2cl,
    0x963ef2c96b33be31l, 0x74f85198b05a2e7dl,
    0x5a0f544dd2b1fb18l, 0x03727073c2e134b1l,
    0xc7f6aa2de59aea61l, 0x352787baa0d7c22fl,
    0x9853eab63b5e0b35l, 0xabbdcdd7ed5c0860l,
    0xcf05daf5ac8d77b0l, 0x49cad48cebf4a71el,
    0x7a4c10ec2158c4a6l, 0xd9e92aa246bf719el,
    0x13ae978d09fe5557l, 0x730499af921549ffl,
    0x4e4b705b92903ba4l, 0xff577222c14f0a3al,
    0x55b6344cf97aafael, 0xb862225b055b6960l,
    0xcac09afbddd2cdb4l, 0xdaf8e9829fe96b5fl,
    0xb5fdfc5d3132c498l, 0x310cb380db6f7503l,
    0xe87fbb46217a360el, 0x2102ae466ebb1148l,
    0xf8549e1a3aa5e00dl, 0x07a69afdcc42261al,
    0xc4c118bfe78feaael, 0xf9f4892ed96bd438l,
    0x1af3dbe25d8f45dal, 0xf5b4b0b0d2deeeb4l,
    0x962aceefa82e1c84l, 0x046e3ecaaf453ce9l,
    0xf05d129681949a4cl, 0x964781ce734b3c84l,
    0x9c2ed44081ce5fbdl, 0x522e23f3925e319el,
    0x177e00f9fc32f791l, 0x2bc60a63a6f3b3f2l,
    0x222bbfae61725606l, 0x486289ddcc3d6780l,
    0x7dc7785b8efdfc80l, 0x8af38731c02ba980l,
    0x1fab64ea29a2ddf7l, 0xe4d9429322cd065al,
    0x9da058c67844f20cl, 0x24c0e332b70019b0l,
    0x233003b5a6cfe6adl, 0xd586bd01c5c217f6l,
    0x5e5637885f29bc2bl, 0x7eba726d8c94094bl,
    0x0a56a5f0bfe39272l, 0xd79476a84ee20d06l,
    0x9e4c1269baa4bf37l, 0x17efee45b0dee640l,
    0x1d95b0a5fcf90bc6l, 0x93cbe0b699c2585dl,
    0x65fa4f227a2b6d79l, 0xd5f9e858292504d5l,
    0xc2b5a03f71471a6fl, 0x59300222b4561e00l,
    0xce2f8642ca0712dcl, 0x7ca9723fbb2e8988l,
    0x2785338347f2ba08l, 0xc61bb3a141e50e8cl,
    0x150f361dab9dec26l, 0x9f6a419d382595f4l,
    0x64a53dc924fe7ac9l, 0x142de49fff7a7c3dl,
    0x0c335248857fa9e7l, 0x0a9c32d5eae45305l,
    0xe6c42178c4bbb92el, 0x71f1ce2490d20b07l,
    0xf1bcc3d275afe51al, 0xe728e8c83c334074l,
    0x96fbf83a12884624l, 0x81a1549fd6573da5l,
    0x5fa7867caf35e149l, 0x56986e2ef3ed091bl,
    0x917f1dd5f8886c61l, 0xd20d8c88c8ffe65fl
  )

  private val castlingMasks = Array(
    0x31d71dce64b2c310l, 0xf165b587df898190l,
    0xa57e6339dd2cf3a0l, 0x1ef6e6dbb1961ec9l
  )

  private val enPassantMasks = Array(
    0x70cc73d90bc26e24l, 0xe21a6b35df0c3ad7l,
    0x003a93d8b2806962l, 0x1c99ded33cb890a1l,
    0xcf3145de0add4289l, 0xd0e4427a5514fb72l,
    0x77c621cc9fb3a483l, 0x67a34dac4356550bl
  )

  private val threeCheckMasks = Array(
    0x1d6dc0ee61ce803el, 0xc6284b653d38e96al,
    0x803f5fb0d2f97fael, 0xb183ccc9e73df9edl,
    0xfdeef11602d6b443l, 0x1b0ce4198b3801a6l
  )

  private val crazyPromotionMasks = Array(
    0x2f9900cc2b7a19cal, 0xf75235beb01886d3l,
    0x8ae7e29889ac9964l, 0xad30091ce7cb4204l,
    0xaae118773ddd4e4dl, 0x8ec514ce4736aa07l,
    0x26a412bd8cef4f15l, 0x1bdce26bd9af059fl,
    0xea5f4ade5acc0516l, 0x69ab7ebc07650565l,
    0x3e655f895a188a1cl, 0xf394f6882a114d65l,
    0x3173cfa2be5bd4d3l, 0x434d20d2ca00ae71l,
    0x3ba297f73d338c93l, 0x099ba1b0205a5ea5l,
    0xc49f050b5e1c5653l, 0xe14eec50a9c690e8l,
    0x2571cc79f4ce0169l, 0xde0f98d6002f4323l,
    0x0682220b02e5c3e8l, 0xcb900d3a6b38c39dl,
    0x24620fbf09d50d66l, 0x0f40a9b2781a119dl,
    0x83c6980df0d04932l, 0xab6f9af720cb5df4l,
    0x1c906974166ee8d4l, 0x9c1ba3db0784ebdal,
    0x81a19098d16aa929l, 0xfce56173c63ccefdl,
    0x43cb7aa20c6209c2l, 0x7e96e2ae86924babl,
    0x01860725034b0fefl, 0xf74d369066ec4e96l,
    0x1ae9962c6e0d1232l, 0x5d66fa465ccfc560l,
    0xe9c13ae1fc36afaal, 0xcaec4035fb840be4l,
    0x839d28adafad0f8fl, 0xe4703b6e30422003l,
    0x1e2fd5b2827d5e43l, 0x96f1e8d8b94bd960l,
    0x90f2075c3f43960cl, 0xc48e0774c4f9134fl,
    0xf17e5f6a2cb000c7l, 0x6248409bf55a4925l,
    0x967bd94eb30505ccl, 0xe91e89853f9e844fl,
    0xb841038e24193f08l, 0x46f3b25cae82a6ccl,
    0x3e97e042449e3ed5l, 0x868a166af46dcbd2l,
    0xf71be788b3fd1a7al, 0xcb6d65410533cc37l,
    0x7e30d70559efaedcl, 0x32db0f5ca18159cel,
    0x97a9116e874228c5l, 0x85ee68ee3a175297l,
    0x076a14170b409e2al, 0xbad49d47dc95855bl,
    0x636187d94ded991el, 0x962e50971f09cfabl,
    0x8f16c910d6776589l, 0x7e3de4bfbef5566fl
  )

  private val crazyPocketMasks = Array(
    0xb262e9f9d6123320l, 0x91533947cdaa8becl,
    0xa13b56b45723a3d4l, 0x9a35cce29ca3ac75l,
    0x2716940e1d4f28d7l, 0x7447209cfb793066l,
    0x5cf91d8ae6402e1al, 0x4625588d38487ac5l,
    0xe42ec6191353e3bdl, 0x478e6cc8f6b2dadal,
    0x1726fc948b994b87l, 0xfb9d2e5a66b46741l,
    0x7f668e401ffe9e6fl, 0xee4d6fe11c46a236l,
    0x006cb70064259959l, 0x33535a7c4def1b24l,
    0x479e792f8171fc29l, 0x656a6e71de970975l,
    0xcada3e48618a1c2bl, 0xb37ad7262db9c99el,
    0x85ae25402a311d5dl, 0x3de4e82d52dbb44cl,
    0xb1c8499674464c21l, 0xf1c1853cc6827b84l,
    0x51f97ed3ba004fb0l, 0x00da9ede878e3e98l,
    0x3cd0fd658e1cdb12l, 0xac2940b688a1d0f9l,
    0xe51acb5b336db0dfl, 0xcf7517fbdcb16174l,
    0xdfe901aba4a2ced3l, 0x24bfd4b72c8852ebl,
    0xf085bcd9711883d4l, 0x41b71908a3d86274l,
    0x6d604cc0a2df1a69l, 0xaedf8291e0048c39l,
    0x09d3c83f59547935l, 0x257d5c7ebc718242l,
    0x56ac1c998f5c2edel, 0xa25c0b0679937316l,
    0xa9a2a7e200faa936l, 0xb8e7ca4716cf9d49l,
    0x9b253f89247c4c1dl, 0x1e701e2a73f9dc4bl,
    0xcdf351b289aa5a84l, 0x2e4e118fc45fdc0dl,
    0x80247d70885ad5cel, 0x0a99dccfce316ca0l,
    0xb5553435dae76840l, 0xee562004d5d14158l,
    0x551b5fa3ec7166a2l, 0x2dbb493c6e9fec06l,
    0xf06b4c65f4bb14a1l, 0x5f0b44d98013acb9l,
    0xce7dbafa734bba8al, 0xe009c0e355a77913l,
    0x21918f473cb6decfl, 0xdcf11e80dc14763fl,
    0x7ac21357500fb0c6l, 0x28abe0a3761e326cl,
    0x30b8e3da17d34c6el, 0xd999d38ffa5d771el,
    0x8a7e0d1367d70b28l, 0x9157bfe7ac071796l,
    0xadda94b21edd779al, 0x6f555cf7856f0d63l,
    0x5b2a5b2788adc947l, 0x500c782c8c562a42l,
    0x20f8b3f7059d8884l, 0x79c890ed3e95f3f4l,
    0xe64dbd474ddcf8cal, 0xa94966fbf7f270d5l,
    0x2473b4e6ad9faa9al, 0x98abdf9fa4b487e6l,
    0x75fa1ecb0717029al, 0xf6053757646a08bal,
    0x060e2788d99813aal, 0x5fa61c63681ebbc8l,
    0x90bbf42db708006al, 0xb525460ec1c15916l,
    0x2696070a4502024dl, 0x158087442731df68l,
    0x65010c3ea0acfdcfl, 0xb28ecdf305a7a831l,
    0xfd037a2e2a2e54e8l, 0x5f09f3763f6a4882l,
    0xe0125e53c4e64b83l, 0x1de44a244be3752al,
    0xf78919dfb05f031cl, 0xbf81caebad91d8e1l,
    0xbc3780dce0bd58d5l, 0x65b5fb1afa5c5714l,
    0xb7ddb798d0c1ff23l, 0xa823d99d1504f4d6l,
    0xa3c526e07f1cf98dl, 0xa848f93e7a83ece4l,
    0x21e3941600abaaecl, 0x534a070449a9238dl,
    0xf86c2e4bb82d3923l, 0xa594b44c256b41f8l,
    0xf1503a710531b677l, 0x171a27a1b9911e11l,
    0xd8d8a26ac022ebe6l, 0x151ca9f641352f33l,
    0x553b499dc1eae685l, 0x0137684bed65e27el,
    0x254a12bd9efa3535l, 0xf8361f0b0a35ef6dl,
    0xa7b7e76b7ff82166l, 0xe266e0067bc7f396l,
    0xbdd8a9037f5d0298l, 0x2d5977c3f88a2a31l,
    0x4587cd651a3bb45fl, 0xbcdc3c56ad971eb0l,
    0x248b2073706e1844l, 0xab03444dfb15bd0al,
    0xbcaff3134756ab78l, 0xeea844cf3e1db285l,
    0xb917fdb80f355116l, 0x21931f559ecefa34l,
    0x7170c6436114a4c2l, 0x73d2a7c1017a2aafl,
    0x3b855d1755dce20el, 0x37e35078817f0dbdl,
    0xe59b1e3389a1aad3l, 0xbad11ebe3c3df239l,
    0xd54aad8a64c65c27l, 0xeadb37a4f7fbeb4cl,
    0x5453586c4984a81cl, 0xb6777cd5e1b16bccl,
    0x24b690161baa0d65l, 0x5bd3613d2ee222fdl,
    0xc928bda035f8c39dl, 0x29e8eeca9b09c735l,
    0xdc35bba3f78ed4eel, 0x1753c5b3ef820c81l,
    0xef3368ab56565ae7l, 0xebf48bd35c4ead40l,
    0x529b39d015e49755l, 0x697ea70620e98751l,
    0xebcabe3d4cbc0212l, 0x2f424e669d2a7f1bl,
    0x6ae47a9c22302f58l, 0x83f9a7b574523121l,
    0x77b6860daa7a39d5l, 0x1611e306f167e512l,
    0x2d78f39e1adbaa9dl, 0x1aada836dedb3ba7l,
    0xf37991753c7df558l, 0xe80840e623a19d08l
  )

  private lazy val h = new Hash(size)

  def apply(situation: Situation): PositionHash = h.apply(situation)

  def debug(hashes: PositionHash) = hashes.map(_.toInt).sum.toString

}
