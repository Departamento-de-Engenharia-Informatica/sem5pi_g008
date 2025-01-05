import {MedicalRecordAllergy} from "../../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {Repo} from "../../core/infra/Repo";
import {MedicalRecordCondition} from "../../domain/MedicalRecordCondition/MedicalRecordCondition";


export default interface IMedicalRecordAllergyRepo extends Repo<MedicalRecordAllergy> {
  save(medicalCondition: MedicalRecordAllergy): Promise<MedicalRecordAllergy>;
  getByMedicalId(medicalRecordId: string): Promise<MedicalRecordAllergy[]>;
  getByDomainId(number: number): Promise<MedicalRecordAllergy>;
  updateUsingDomainId(allergy: any, description: string): Promise<MedicalRecordAllergy>;


}
