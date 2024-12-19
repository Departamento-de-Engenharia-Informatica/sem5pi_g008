import IAllergyDTO from "../../dto/IAllergyDTO";

export default interface IAllergyService {
  createAllergy(allergy: IAllergyDTO): Promise<any>;
}
