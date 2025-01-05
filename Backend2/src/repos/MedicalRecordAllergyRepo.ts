import {Inject, Service} from "typedi";
import {Document, Model} from "mongoose";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {MedicalRecordAllergyMapper} from "../mappers/MedicalRecordAllergyMapper";
import IMedicalRecordAllergyRepo from "../services/IRepos/IMedicalRecordAllergyRepo";
import {IMedicalRecordAllergyPersistence} from "../dataschema/IMedicalRecordAllergyPersistence";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import {NotFoundException} from "../Exceptions/NotFoundException";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";


@Service()
export default class MedicalRecordAllergyRepo implements IMedicalRecordAllergyRepo{
  constructor(@Inject('medicalRecordAllergySchema') private medicalRecordAllergySchema: Model<IMedicalRecordAllergyPersistence & Document>,) {
  }

  exists(t: MedicalRecordAllergy): Promise<boolean> {
    //TODO:Implement exists method
    return Promise.resolve(false);
  }

  public async save(medicalRecordAllergy: MedicalRecordAllergy): Promise<MedicalRecordAllergy> {

    const id = await this.getLastId();

    const rawMedicalRecordAllergy: any = MedicalRecordAllergyMapper.toPersistence(medicalRecordAllergy, id);

    const medicalRecordAllergyCreated = await this.medicalRecordAllergySchema.create(rawMedicalRecordAllergy);

    return MedicalRecordAllergyMapper.toDomain(medicalRecordAllergyCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordAllergySchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }

  public async getByMedicalId(medicalRecordId: string): Promise<MedicalRecordAllergy[]> {
    const medicalRecordAllergies = await this.medicalRecordAllergySchema
      .find({medicalRecordId: medicalRecordId})
      .exec();

    return medicalRecordAllergies.map(MedicalRecordAllergyMapper.toDomain);
  }

  public async getByDomainId(number: number): Promise<MedicalRecordAllergy> {
    const allergy = await this.medicalRecordAllergySchema.findOne({domainId: number});

    if (!allergy) {
      throw new NotFoundException("allergy not found");
    }

    return MedicalRecordAllergyMapper.toDomain(allergy);
  }

  public async updateUsingDomainId(allergy: any, comment: string): Promise<MedicalRecordAllergy> {
    const domainId = allergy.domainId;
    const rawAllergy = MedicalRecordAllergyMapper.toPersistence(allergy, domainId);
    delete rawAllergy.domainId;
    delete rawAllergy.doctorId;
    delete rawAllergy.allergyId;
    delete rawAllergy.medicalRecordId;

    const remainingFields = ['comment'];

    // for (const field of comment) {
    //   console.log("field", field)
    //   if (!remainingFields.includes(field)) {
    //     throw new Error("Invalid field to update");
    //   }
    // }
    Object.keys(rawAllergy).forEach((key) => {
      if (!comment.includes(key)) {
        delete rawAllergy[key];
      }
    });
    const updatedAllergy = await this.medicalRecordAllergySchema.findOneAndUpdate(
        {domainId: domainId},
        rawAllergy,
        {new: true}
    );
    if (!updatedAllergy) {
      throw new NotFoundException("Allergy not found");
    }

    return MedicalRecordAllergyMapper.toDomain(updatedAllergy);
  }
}
